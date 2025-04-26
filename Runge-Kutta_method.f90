PROGRAM rk4_parallel
  USE mpi
  IMPLICIT NONE

  INTEGER :: n, m, block_size, i, j, t, ierr, rank, size, steps
  REAL(8), ALLOCATABLE :: P(:,:), P_new(:,:), A(:,:), k1(:,:), k2(:,:), k3(:,:), k4(:,:), temp(:,:)
  REAL(8) :: epsilon, dt, t_max, t_val
  INTEGER :: status(MPI_STATUS_SIZE)

  ! MPI初期化
  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

  ! パラメータ設定
  n = 100           ! 行列サイズ
  m = size          ! ブロック数（プロセス数）
  block_size = n / m
  epsilon = 0.1     ! 非線形項の強さ
  dt = 0.01         ! 時間刻み
  t_max = 1.0       ! 最大時間
  steps = INT(t_max / dt)

  ! 配列の割り当て
  ALLOCATE(P(block_size, n), P_new(block_size, n), A(n, n), k1(block_size, n), k2(block转变, n), &
           k3(block_size, n), k4(block_size, n), temp(block_size, n))

  ! 初期確率行列（例：一様分布）
  P = 1.0 / REAL(n)
  A = 0.0
  ! 生成行列Aの設定（例：ランダムウォーク）
  IF (rank == 0) THEN
    DO i = 1, n
      A(i,i) = -2.0
      IF (i > 1) A(i,i-1) = 1.0
      IF (i < n) A(i,i+1) = 1.0
    END DO
  END IF
  CALL MPI_BCAST(A, n*n, MPI_DOUBLE, 0, MPI_COMM_WORLD, ierr)

  ! 時間発展
  DO t = 1, steps
    t_val = REAL(t-1) * dt

    ! k1 = f(t, P)
    CALL compute_f(P, A, n, block_size, epsilon, k1)

    ! k2 = f(t + dt/2, P + dt/2 * k1)
    temp = P + 0.5 * dt * k1
    CALL compute_f(temp, A, n, block_size, epsilon, k2)

    ! k3 = f(t + dt/2, P + dt/2 * k2)
    temp = P + 0.5 * dt * k2
    CALL compute_f(temp, A, n, block_size, epsilon, k3)

    ! k4 = f(t + dt, P + dt * k3)
    temp = P + dt * k3
    CALL compute_f(temp, A, n, block_size, epsilon, k4)

    ! 更新
    P_new = P + (dt / 6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

    ! 正規化
    DO i = 1, block_size
      P_new(i,:) = P_new(i,:) / SUM(P_new(i,:))
    END DO

    P = P_new
  END DO

  ! 結果出力（例：rank 0が最終状態を出力）
  IF (rank == 0) THEN
    WRITE(*,*) "Final P(1,1) = ", P(1,1)
  END IF

  ! メモリ解放
  DEALLOCATE(P, P_new, A, k1, k2, k3, k4, temp)

  ! MPI終了
  CALL MPI_FINALIZE(ierr)

CONTAINS

  ! 関数 f(t, P) = A*P + epsilon*(P^2 - P)
  SUBROUTINE compute_f(P_in, A, n, block_size, epsilon, f_out)
    INTEGER, INTENT(IN) :: n, block_size
    REAL(8), INTENT(IN) :: P_in(block_size,n), A(n,n), epsilon
    REAL(8), INTENT(OUT) :: f_out(block_size,n)
    REAL(8) :: P_sq(block_size,n)

    ! 線形項：A * P
    CALL DGEMM('N', 'N', block_size, n, n, 1.0D0, P_in, block_size, A, n, 0.0D0, f_out, block_size)

    ! 非線形項：P^2 - P
    CALL DGEMM('N', 'N', block_size, n, n, 1.0D0, P_in, block_size, P_in, block_size, 0.0D0, P_sq, block_size)
    f_out = f_out + epsilon * (P_sq - P_in)
  END SUBROUTINE compute_f

END PROGRAM rk4_parallel
