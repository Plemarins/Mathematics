PROGRAM StabilityAnalysis
  IMPLICIT NONE
  INTEGER, PARAMETER :: n = 6  ! 状態数
  REAL(8), DIMENSION(n,n) :: P  ! 確率行列
  REAL(8), DIMENSION(n) :: pi, pi_next, pi_star  ! 確率分布
  REAL(8), DIMENSION(n) :: system_vals, stress_vals  ! 状態の数値化
  REAL(8), DIMENSION(10) :: cov_history  ! 共分散の履歴
  INTEGER :: t, steps
  REAL(8) :: P_stress_high_given_down, cov
  LOGICAL :: symmetry

  ! 確率行列Pの初期化
  CALL InitializeMatrix(P)
  ! 初期分布piの設定
  pi = (/ 0.5D0, 0.3D0, 0.1D0, 0.05D0, 0.03D0, 0.02D0 /)
  ! 状態の数値化（system: up=1, down=0; stress: low=0, medium=1, high=2）
  system_vals = (/ 1.0D0, 1.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0 /)
  stress_vals = (/ 0.0D0, 1.0D0, 2.0D0, 0.0D0, 1.0D0, 2.0D0 /)
  steps = 10

  ! 1. 変換：時間発展
  WRITE(*,*) 'Time evolution of probability distribution:'
  pi_next = pi
  DO t = 1, steps
    CALL MatrixVectorMult(P, pi_next, pi)
    pi_next = pi
    WRITE(*,'(A,I2,A,6F8.4)') 't=', t, ': ', pi_next
    ! 共分散計算（後で使用）
    cov = ComputeCovariance(pi_next, system_vals, stress_vals)
    cov_history(t) = cov
  END DO

  ! 2. 不変性：定常分布
  CALL StationaryDistribution(P, pi_star)
  WRITE(*,*) 'Stationary distribution:'
  WRITE(*,'(6F8.4)') pi_star

  ! 3. 対称性：ストレス遷移の対称性チェック
  symmetry = ABS(P(1,2) - P(4,5)) < 1.0D-5  ! (up,low)->(up,medium) vs (down,low)->(down,medium)
  WRITE(*,*) 'Symmetry in stress transition: ', symmetry

  ! 4. 相互作用：システムdown時のストレスhigh確率
  P_stress_high_given_down = SUM(P(:,6)) / SUM(P(:,4:6))
  WRITE(*,*) 'P(stress=high | system=down): ', P_stress_high_given_down

  ! 5. 共変性：共分散の履歴
  WRITE(*,*) 'Covariance over time:'
  DO t = 1, steps
    WRITE(*,'(A,I2,A,F8.4)') 't=', t, ': ', cov_history(t)
  END DO

CONTAINS

  ! 確率行列Pの初期化
  SUBROUTINE InitializeMatrix(P)
    REAL(8), DIMENSION(n,n), INTENT(OUT) :: P
    P(1,:) = (/ 0.7D0, 0.2D0, 0.05D0, 0.03D0, 0.01D0, 0.01D0 /)  ! (up,low)
    P(2,:) = (/ 0.1D0, 0.6D0, 0.2D0, 0.05D0, 0.03D0, 0.02D0 /)   ! (up,medium)
    P(3,:) = (/ 0.05D0, 0.1D0, 0.7D0, 0.05D0, 0.05D0, 0.05D0 /)  ! (up,high)
    P(4,:) = (/ 0.1D0, 0.05D0, 0.05D0, 0.6D0, 0.1D0, 0.1D0 /)    ! (down,low)
    P(5,:) = (/ 0.05D0, 0.1D0, 0.05D0, 0.1D0, 0.6D0, 0.1D0 /)    ! (down,medium)
    P(6,:) = (/ 0.01D0, 0.01D0, 0.1D0, 0.1D0, 0.1D0, 0.68D0 /)    ! (down,high)
  END SUBROUTINE InitializeMatrix

  ! 行列ベクトル積：pi_new = pi ・ P
  SUBROUTINE MatrixVectorMult(P, pi, pi_new)
    REAL(8), DIMENSION(n,n), INTENT(IN) :: P
    REAL(8), DIMENSION(n), INTENT(IN) :: pi
    REAL(8), DIMENSION(n), INTENT(OUT) :: pi_new
    INTEGER :: i, j
    pi_new = 0.0D0
    DO i = 1, n
      DO j = 1, n
        pi_new(i) = pi_new(i) + pi(j) * P(j,i)
      END DO
    END DO
  END SUBROUTINE MatrixVectorMult

  ! 定常分布の計算（簡略化：P^T - Iの解を近似）
  SUBROUTINE StationaryDistribution(P, pi_star)
    REAL(8), DIMENSION(n,n), INTENT(IN) :: P
    REAL(8), DIMENSION(n), INTENT(OUT) :: pi_star
    REAL(8), DIMENSION(n,n) :: A
    REAL(8), DIMENSION(n) :: b
    INTEGER :: i
    ! P^T - Iを構築
    A = TRANSPOSE(P)
    DO i = 1, n
      A(i,i) = A(i,i) - 1.0D0
    END DO
    ! 最後の行をΣπ=1に置き換え
    A(n,:) = 1.0D0
    b = 0.0D0
    b(n) = 1.0D0
    ! 簡単のため、初期値を仮定して反復で近似
    pi_star = 1.0D0 / n
    DO i = 1, 100  ! 反復回数
      CALL MatrixVectorMult(P, pi_star, pi_star)
      pi_star = pi_star / SUM(pi_star)  ! 正規化
    END DO
  END SUBROUTINE StationaryDistribution

  ! 共分散の計算
  REAL(8) FUNCTION ComputeCovariance(pi, system_vals, stress_vals)
    REAL(8), DIMENSION(n), INTENT(IN) :: pi, system_vals, stress_vals
    REAL(8) :: E_system, E_stress, E_system_stress
    INTEGER :: i
    E_system = 0.0D0
    E_stress = 0.0D0
    E_system_stress = 0.0D0
    DO i = 1, n
      E_system = E_system + pi(i) * system_vals(i)
      E_stress = E_stress + pi(i) * stress_vals(i)
      E_system_stress = E_system_stress + pi(i) * system_vals(i) * stress_vals(i)
    END DO
    ComputeCovariance = E_system_stress - E_system * E_stress
  END FUNCTION ComputeCovariance

END PROGRAM StabilityAnalysis
