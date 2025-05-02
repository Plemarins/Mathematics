PROGRAM SensorimotorCausalDiagram
  IMPLICIT NONE
  INTEGER, PARAMETER :: N_MODES = 3 ! HMMモード数
  INTEGER, PARAMETER :: N_NODES = 5 ! DBNノード数（目標x,y,アームtheta1,theta2,障害物距離）
  INTEGER, PARAMETER :: N_CONTEXT = 5 ! コンテキスト履歴
  INTEGER, PARAMETER :: N_STATE = 4 ! ダイナミックシステム状態
  INTEGER, PARAMETER :: N_HIDDEN = 20 ! LSTM隠れユニット
  INTEGER, PARAMETER :: N_PARTICLES = 10 ! DBN粒子数
  REAL :: hmm_trans(N_MODES, N_MODES), hmm_mean(N_MODES, 2), hmm_var(N_MODES) ! HMMパラメータ
  REAL :: dbn_particles(N_PARTICLES, N_NODES), dbn_weights(N_PARTICLES) ! DBN粒子
  REAL :: context(4, N_CONTEXT) ! コンテキスト（目標x,y + 障害物x,y）
  REAL :: state(N_STATE) ! 状態（theta1, theta2, dtheta1, dtheta2）
  REAL :: lstm_hidden(N_HIDDEN), lstm_cell(N_HIDDEN) ! LSTM隠れ状態とセル
  REAL :: lstm_input(4), lstm_output(2) ! LSTM入出力
  REAL :: lstm_Wih(N_HIDDEN, 4), lstm_Whh(N_HIDDEN, N_HIDDEN), lstm_Who(2, N_HIDDEN) ! LSTM重み
  REAL :: target(2), obstacle(2), motor_output(2), sensor_input(4) ! 入力と出力
  REAL :: weights(5) ! フレームワーク信頼度
  INTEGER :: current_mode, t
  REAL :: dt = 0.01, noise = 0.1 ! 時間ステップとノイズ
  REAL :: error, total_error = 0.0

  ! 初期化
  CALL RANDOM_SEED()
  hmm_trans = RESHAPE([0.7, 0.2, 0.1, 0.3, 0.5, 0.2, 0.1, 0.3, 0.6], [N_MODES, N_MODES])
  hmm_mean = RESHAPE([0.0, 0.0, 0.5, 0.5, -0.5, -0.5], [N_MODES, 2])
  hmm_var = [0.1, 0.2, 0.3]
  dbn_particles = 0.0
  dbn_weights = 1.0 / N_PARTICLES
  context = 0.0
  state = [0.0, 0.0, 0.0, 0.0]
  lstm_hidden = 0.0
  lstm_cell = 0.0
  lstm_Wih = 0.05
  lstm_Whh = 0.05
  lstm_Who = 0.05
  current_mode = 1
  sensor_input = [1.0, 1.0, 0.0, 0.0]
  weights = [0.2, 0.2, 0.2, 0.2, 0.2]
  OPEN(UNIT=10, FILE='trajectory.dat', STATUS='REPLACE')

  ! 時間ステップのループ
  DO t = 1, 200
    ! 目標と障害物の更新
    target(1) = COS(REAL(t) * 0.05)
    target(2) = SIN(REAL(t) * 0.05)
    obstacle(1) = 0.5 * COS(REAL(t) * 0.1)
    obstacle(2) = 0.5 * SIN(REAL(t) * 0.1)
    sensor_input(1:2) = target + GaussianNoise(noise)
    sensor_input(3:4) = obstacle

    ! 因果ダイアグラムの更新
    ! 1. HMM: モード選択
    CALL HMMUpdate(current_mode, hmm_trans, hmm_mean, hmm_var, sensor_input, state(1:2))
    weights(1) = HMMConfidence(current_mode, hmm_trans)

    ! 2. DBN: 粒子フィルタリング
    motor_output = DBNInfer(sensor_input, state(1:2), dbn_particles, dbn_weights)
    weights(2) = SUM(dbn_weights) / N_PARTICLES

    ! 3. コンテキスト学習
    CALL UpdateContext(context, sensor_input)
    motor_output = ContextLearning(context, motor_output)
    weights(3) = ContextConfidence(context)

    ! 4. ダイナミックシステム
    CALL DynamicSystem(state, motor_output, sensor_input(3:4), dt)
    weights(4) = 0.2

    ! 5. LSTM
    lstm_input = sensor_input
    CALL LSTMForward(lstm_input, lstm_hidden, lstm_cell, lstm_output, lstm_Wih, lstm_Whh, lstm_Who)
    weights(5) = 0.2

    ! 動的加重統合
    motor_output = weights(2) * motor_output + weights(5) * lstm_output
    motor_output = motor_output / SUM(weights(2:5:3))

    ! 誤差計算
    error = SQRT(SUM((target - ForwardKinematics(state(1:2)))**2))
    total_error = total_error + error
    WRITE(10, *) t, state(1:2), target, obstacle, error
    PRINT *, 'Time:', t, 'Mode:', current_mode, 'Error:', error
  END DO

  PRINT *, 'Average Error:', total_error / 200
  CLOSE(10)

CONTAINS

  ! ガウスノイズ
  FUNCTION GaussianNoise(sigma) RESULT(noise)
    REAL, INTENT(IN) :: sigma
    REAL :: noise(2), u1, u2
    CALL RANDOM_NUMBER(u1)
    CALL RANDOM_NUMBER(u2)
    noise(1) = sigma * SQRT(-2.0 * LOG(u1)) * COS(2.0 * 3.14159 * u2)
    noise(2) = sigma * SQRT(-2.0 * LOG(u1)) * SIN(2.0 * 3.14159 * u2)
  END FUNCTION GaussianNoise

  ! HMM: モード更新
  SUBROUTINE HMMUpdate(mode, trans, mean, var, input, state)
    INTEGER, INTENT(INOUT) :: mode
    REAL, INTENT(IN) :: trans(N_MODES, N_MODES), mean(N_MODES, 2), var(N_MODES), input(4), state(2)
    REAL :: prob(N_MODES), cum_prob, rand, obs_prob
    INTEGER :: i
    CALL RANDOM_NUMBER(rand)
    prob = 0.0
    DO i = 1, N_MODES
      obs_prob = EXP(-SUM((input(1:2) - state - mean(i, :))**2) / (2.0 * var(i)))
      prob(i) = trans(mode, i) * obs_prob
    END DO
    prob = prob / SUM(prob)
    cum_prob = 0.0
    DO i = 1, N_MODES
      cum_prob = cum_prob + prob(i)
      IF (rand < cum_prob) THEN
        mode = i
        EXIT
      END IF
    END DO
  END SUBROUTINE HMMUpdate

  ! HMM: 信頼度
  REAL FUNCTION HMMConfidence(mode, trans)
    INTEGER, INTENT(IN) :: mode
    REAL, INTENT(IN) :: trans(N_MODES, N_MODES)
    HMMConfidence = MAXVAL(trans(mode, :))
  END FUNCTION HMMConfidence

  ! DBN: 粒子フィルタリング
  FUNCTION DBNInfer(input, state, particles, weights) RESULT(output)
    REAL, INTENT(IN) :: input(4), state(2)
    REAL, INTENT(INOUT) :: particles(N_PARTICLES, N_NODES), weights(N_PARTICLES)
    REAL :: output(2), new_particles(N_PARTICLES, N_NODES)
    INTEGER :: i
    DO i = 1, N_PARTICLES
      new_particles(i, 1:2) = input(1:2) + GaussianNoise(0.05)
      new_particles(i, 3:4) = state
      new_particles(i, 5) = NORM2(input(3:4) - ForwardKinematics(state))
      weights(i) = EXP(-NORM2(new_particles(i, 1:2) - input(1:2))**2 / 0.1)
    END DO
    weights = weights / SUM(weights)
    particles = new_particles
    output = SUM(particles(:, 3:4) * SPREAD(weights, 2, 2), 1)
  END FUNCTION DBNInfer

  ! コンテキスト学習: 更新
  SUBROUTINE UpdateContext(context, input)
    REAL, INTENT(INOUT) :: context(4, N_CONTEXT)
    REAL, INTENT(IN) :: input(4)
    context(:, 1:N_CONTEXT-1) = context(:, 2:N_CONTEXT)
    context(:, N_CONTEXT) = input
  END SUBROUTINE UpdateContext

  ! コンテキスト学習: 注意ベース
  FUNCTION ContextLearning(context, base_output) RESULT(output)
    REAL, INTENT(IN) :: context(4, N_CONTEXT), base_output(2)
    REAL :: output(2), attention(N_CONTEXT), scores(N_CONTEXT)
    INTEGER :: i
    scores = 0.0
    DO i = 1, N_CONTEXT
      scores(i) = SUM(context(:, i) * context(:, N_CONTEXT))
    END DO
    attention = EXP(scores) / SUM(EXP(scores))
    output = base_output
    DO i = 1, N_CONTEXT
      output = output + attention(i) * context(1:2, i)
    END DO
    output = output / (SUM(attention) + 1.0)
  END FUNCTION ContextLearning

  ! コンテキスト: 信頼度
  REAL FUNCTION ContextConfidence(context)
    REAL, INTENT(IN) :: context(4, N_CONTEXT)
    ContextConfidence = 0.2
  END FUNCTION ContextConfidence

  ! ダイナミックシステム
  SUBROUTINE DynamicSystem(state, input, obstacle, dt)
    REAL, INTENT(INOUT) :: state(N_STATE)
    REAL, INTENT(IN) :: input(2), obstacle(2), dt
    REAL :: torque(2), Kp = 2.0, Kd = 0.5, dist, avoidance(2)
    REAL :: end_effector(2)
    end_effector = ForwardKinematics(state(1:2))
    dist = NORM2(end_effector - obstacle)
    avoidance = 0.0
    IF (dist < 0.3) avoidance = 0.1 * (end_effector - obstacle) / dist
    torque = Kp * (input - state(1:2)) - Kd * state(3:4) + avoidance
    state(3:4) = state(3:4) + dt * torque
    state(1:2) = state(1:2) + dt * state(3:4)
  END SUBROUTINE DynamicSystem

  ! 順運動学
  FUNCTION ForwardKinematics(theta) RESULT(pos)
    REAL, INTENT(IN) :: theta(2)
    REAL :: pos(2), L1 = 1.0, L2 = 1.0
    pos(1) = L1 * COS(theta(1)) + L2 * COS(theta(1) + theta(2))
    pos(2) = L1 * SIN(theta(1)) + L2 * SIN(theta(1) + theta(2))
  END FUNCTION ForwardKinematics

  ! LSTM: 順伝播
  SUBROUTINE LSTMForward(input, hidden, cell, output, Wih, Whh, Who)
    REAL, INTENT(IN) :: input(4), Wih(N_HIDDEN, 4), Whh(N_HIDDEN, N_HIDDEN), Who(2, N_HIDDEN)
    REAL, INTENT(INOUT) :: hidden(N_HIDDEN), cell(N_HIDDEN), output(2)
    REAL :: forget(N_HIDDEN), input_gate(N_HIDDEN), output_gate(N_HIDDEN), candidate(N_HIDDEN)
    INTEGER :: i, j
    DO i = 1, N_HIDDEN
      forget(i) = 0.0
      input_gate(i) = 0.0
      output_gate(i) = 0.0
      candidate(i) = 0.0
      DO j = 1, 4
        forget(i) = forget(i) + Wih(i, j) * input(j)
        input_gate(i) = input_gate(i) + Wih(i, j) * input(j)
        output_gate(i) = output_gate(i) + Wih(i, j) * input(j)
        candidate(i) = candidate(i) + Wih(i, j) * input(j)
      END DO
      DO j = 1, N_HIDDEN
        forget(i) = forget(i) + Whh(i, j) * hidden(j)
        input_gate(i) = input_gate(i) + Whh(i, j) * hidden(j)
        output_gate(i) = output_gate(i) + Whh(i, j) * hidden(j)
        candidate(i) = candidate(i) + Whh(i, j) * hidden(j)
      END DO
      forget(i) = 1.0 / (1.0 + EXP(-forget(i)))
      input_gate(i) = 1.0 / (1.0 + EXP(-input_gate(i)))
      output_gate(i) = 1.0 / (1.0 + EXP(-output_gate(i)))
      candidate(i) = TANH(candidate(i))
    END DO
    cell = forget * cell + input_gate * candidate
    hidden = output_gate * TANH(cell)
    output = 0.0
    DO i = 1, N_HIDDEN
      output = output + Who(:, i) * hidden(i)
    END DO
  END SUBROUTINE LSTMForward

  ! ユークリッド距離
  REAL FUNCTION NORM2(vec)
    REAL, INTENT(IN) :: vec(2)
    NORM2 = SQRT(SUM(vec**2))
  END FUNCTION NORM2

END PROGRAM SensorimotorCausalDiagram
