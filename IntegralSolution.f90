PROGRAM fmm_parallel
  USE mpi
  IMPLICIT NONE

  INTEGER :: n, m, block_size, i, j, t, ierr, rank, size, steps, L
  REAL(8), ALLOCATABLE :: P(:,:), P0(:,:), K(:,:), M(:,:), T(:,:), temp(:,:), x(:,:)
  REAL(8) :: epsilon, dt, t_max, t_val
  INTEGER :: status(MPI_STATUS_SIZE)
  INTEGER, ALLOCATABLE :: clusters(:,:)
  INTEGER :: cluster_size

  ! MPI初期化
  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

  ! パラメータ設定
  n = 1000          ! 点数
  m = size          ! クラスタ数（プロセス数）
  block_size = n / m
  L = 10            ! 多重極展開の次数
  epsilon = 0.1     ! 非線形項の強さ
  dt = 0.01         ! 時間刻み
  t_max = 1.0       ! 最大時間
  steps = INT(t_max / dt)
  cluster_size = n / m  ! クラスタごとの点数（簡略化）

  ! 配列の割り当て
  ALLOCATE(P(block_size, n), P0(block_size, n), K(n, n), M(block_size, L), T(block_size, L), &
           temp(block_size, n), x(n, 2), clusters(m, cluster_size))

  ! 初期化（一様分布）
  P = 1.0 / REAL(n)
  P0 = 1.0 / REAL(n)

  ! 空間座標（2D、[0,1]^2）
  IF (rank == 0) THEN
    DO i = 1, n
      x(i, 1) = REAL(MOD(i-1, INT(SQRT(REAL(n))))) / SQRT(REAL(n))
      x(i, 2) = REAL((i-1) / INT(SQRT(REAL(n)))) / SQRT(REAL(n))
    END DO
  END IF
  CALL MPI_BCAST(x, n*2, MPI_DOUBLE, 0, MPI_COMM_WORLD, ierr)

  ! カーネル（ガウスカーネル）
  IF (rank == 0) THEN
    DO i = 1, n
      DO j = 1, n
        K(i,j) = EXP(-SUM((x(i,:)-x(j,:))**2))
      END DO
    END DO
  END IF
  CALL MPI_BCAST(K, n*n, MPI_DOUBLE, 0, MPI_COMM_WORLD, ierr)

  ! クラスタ設定（簡略化：均等分割）
  DO i = 1, block_size
    clusters(rank+1, i) = rank * block_size + i
  END DO
  CALL MPI_BCAST(clusters, m*cluster_size, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

  ! 時間発展
  DO t = 1, steps
    t_val = REAL(t-1) * dt

    ! モーメント計算（遠方場）
    CALL compute_moments(P, K, x, clusters, block_size, L, rank, M)

    ! 変換項計算
    CALL compute_transfers(M, x, clusters, block_size, L, rank, T)

    ! 近傍場計算
    CALL compute_near_field(P, K, x, clusters, block_size, rank, temp)

    ! 積分結果
    DO i = 1, block_size
      DO j = 1, n
        P0(i,j) = 1.0 / REAL(n)  ! 簡略化（実際は時間依存）
        P(i,j) = P0(i,j) + epsilon * (T(i,j) + temp(i,j))
      END DO
    END DO

    ! 正規化
    DO i = 1, block_size
      P(i,:) = P(i,:) / SUM(P(i,:))
    END DO
  END DO

  ! 結果出力
  IF (rank == 0) THEN
    WRITE(*,*) "Final P(1,1) = ", P(1,1)
  END IF

  ! メモリ解放
  DEALLOCATE(P, P0, K, M, T, temp, x, clusters)

  ! MPI終了
  CALL MPI_FINALIZE(ierr)

CONTAINS

  ! モーメント計算（ダミー実装）
  SUBROUTINE compute_moments(P, K, x, clusters, block_size, L, rank, M)
    INTEGER, INTENT(IN) :: block_size, L, rank, clusters(:,:)
    REAL(8), INTENT(IN) :: P(block_size,n), K(n,n), x(n,2)
    REAL(8), INTENT(OUT) :: M(block_size,L)
    INTEGER :: i, l
    DO i = 1, block_size
      DO l = 1, L
        M(i,l) = SUM(P(i,:) * K(clusters(rank+1,i),:))  ! 簡略化
      END DO
    END DO
  END SUBROUTINE compute_moments

  ! 変換項計算（ダミー実装）
  SUBROUTINE compute_transfers(M, x, clusters, block_size, L, rank, T)
    INTEGER, INTENT(IN) :: block_size, L, rank, clusters(:,:)
    REAL(8), INTENT(IN) :: M(block_size,L), x(n,2)
    REAL(8), INTENT(OUT) :: T(block_size,L)
    INTEGER :: i, l
    DO i = 1, block_size
      DO l = 1, L
        T(i,l) = M(i,l)  ! 簡略化（実際は球面調和関数）
      END DO
    END DO
  END SUBROUTINE compute_transfers

  ! 近傍場計算
  SUBROUTINE compute_near_field(P, K, x, clusters, block_size, rank, near)
    INTEGER, INTENT(IN) :: block_size, rank, clusters(:,:)
    REAL(8), INTENT(IN) :: P(block_size,n), K(n,n), x(n,2)
    REAL(8), INTENT(OUT) :: near(block_size,n)
    INTEGER :: i, j, k
    REAL(8) :: P_sq(block_size,n)
    DO i = 1, block_size
      ! P^2 の計算
      CALL DGEMM('N', 'N', block_size, n, n, 1.0D0, P, block_size, P, block_size, 0.0D0, P_sq, block_size)
      DO j = 1, n
        near(i,j) = 0.0
        ! 近傍クラスタのみ計算（簡略化）
        DO k = 1, block_size
          near(i,j) = near(i,j) + K(clusters(rank+1,i), clusters(rank+1,k)) * (P_sq(k,j) - P(k,j))
        END DO
      END DO
    END DO
  END SUBROUTINE compute_near_field

END PROGRAM fmm_parallel
