program ammonia_synthesis
  use mpi
  implicit none

  integer, parameter :: n = 3        ! 化学種の数 (N2, H2, NH3)
  integer, parameter :: steps = 100  ! 時間ステップ数
  real(8), parameter :: kappa = 2.0  ! 触媒効率
  real(8), parameter :: E_max = 100.0 ! エネルギー制約
  real(8) :: X(n), X_new(n)         ! 濃度ベクトル
  real(8) :: P(n,n)                 ! 遷移行列
  real(8) :: f(n)                   ! 非線形項
  real(8) :: epsilon(n)             ! ノイズ項
  real(8) :: E_total                ! 累積エネルギー消費
  integer :: i, j, t, rank, size, ierr
  real(8) :: norm_P                 ! フロベニウスノルム

  ! MPI初期化
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

  ! 初期濃度設定（N2=1.0, H2=3.0, NH3=0.0）
  X = [1.0d0, 3.0d0, 0.0d0]
  E_total = 0.0d0

  ! 時間ステップのループ
  do t = 1, steps
    ! 遷移行列Pの計算（触媒効果を考慮）
    call compute_transition_matrix(X, P, kappa, n)
    
    ! 非線形項の計算
    call compute_nonlinear_term(X, f, n)
    
    ! ノイズ項の計算
    call compute_noise(epsilon, n)
    
    ! 新しい濃度の計算（並列化）
    X_new = 0.0d0
    do i = 1, n
      do j = 1, n
        X_new(i) = X_new(i) + P(i,j) * X(j)
      end do
      X_new(i) = X_new(i) + f(i) + epsilon(i)
    end do
    
    ! エネルギー消費の計算（フロベニウスノルム）
    norm_P = 0.0d0
    do i = 1, n
      do j = 1, n
        norm_P = norm_P + P(i,j)**2
      end do
    end do
    norm_P = sqrt(norm_P)
    E_total = E_total + norm_P**2
    
    ! 濃度更新
    X = X_new
    
    ! プロセッサ0で結果出力
    if (rank == 0) then
      write(*,'(A,I4,A,3F8.4)') 'Step ', t, ': X = ', X
      write(*,'(A,F8.4)') 'Energy consumption: ', E_total
    end if
    
    ! エネルギー制約チェック
    if (E_total > E_max) then
      if (rank == 0) write(*,*) 'Energy limit exceeded!'
      exit
    end if
  end do

  ! MPI終了
  call MPI_FINALIZE(ierr)

contains

  ! 遷移行列の計算
  subroutine compute_transition_matrix(X, P, kappa, n)
    integer, intent(in) :: n
    real(8), intent(in) :: X(n), kappa
    real(8), intent(out) :: P(n,n)
    integer :: i, j
    real(8) :: sum_k
    P = 0.0d0
    do i = 1, n
      sum_k = 0.0d0
      do j = 1, n
        sum_k = sum_k + 0.1d0 * X(j)  ! 反応速度定数の簡略化
      end do
      do j = 1, n
        P(i,j) = kappa * (0.1d0 * X(i) * X(j)) / (1.0d0 + sum_k)
      end do
    end do
  end subroutine

  ! 非線形項の計算
  subroutine compute_nonlinear_term(X, f, n)
    integer, intent(in) :: n
    real(8), intent(in) :: X(n)
    real(8), intent(out) :: f(n)
    integer :: i
    do i = 1, n
      f(i) = -0.1d0 * X(i)**2  ! 二次非線形項
    end do
  end subroutine

  ! ノイズ項の計算（簡易乱数）
  subroutine compute_noise(epsilon, n)
    integer, intent(in) :: n
    real(8), intent(out) :: epsilon(n)
    integer :: i
    call random_seed()
    do i = 1, n
      call random_number(epsilon(i))
      epsilon(i) = (epsilon(i) - 0.5d0) * 0.01d0  ! 小さなノイズ
    end do
  end subroutine

end program ammonia_synthesis
