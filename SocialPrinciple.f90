program happiness_simulation
  use mpi
  implicit none

  ! パラメータ定義
  integer, parameter :: dp = kind(1.0d0)
  real(dp), parameter :: T = 100.0_dp, dt = 0.1_dp, sigma = 0.1_dp, Delta = 0.5_dp
  real(dp), parameter :: w(3) = [0.4_dp, 0.3_dp, 0.2_dp], lambda = 0.5_dp, hbar = 1.0_dp, g = 0.1_dp
  integer, parameter :: n_steps = int(T / dt) + 1, n_dim = 3

  ! MPI関連
  integer :: ierr, rank, n_procs
  integer :: status(MPI_STATUS_SIZE)

  ! 変数
  real(dp) :: x(n_dim), x_big(n_dim), x_new(n_dim), A_mu(n_dim), b(n_dim)
  complex(dp) :: psi(n_dim), psi_big(n_dim), psi_new(n_dim), H0(n_dim, n_dim), H_BIG(n_dim, n_dim)
  real(dp) :: H_classical(n_steps), H_classical_big(n_steps), H_quantum(n_steps), H_quantum_big(n_steps)
  real(dp) :: H_total(n_steps), H_total_big(n_steps)
  real(dp) :: t, rand_vec(n_dim)
  integer :: i, j, k

  ! 外部関数
  external :: dgemv, zgemv
  real(dp), external :: happiness_classical
  complex(dp), external :: happiness_quantum

  ! MPI初期化
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, n_procs, ierr)

  ! 乱数初期化
  call random_seed()

  ! 初期状態
  x = 0.0_dp
  x_big = 0.0_dp
  psi = cmplx(1.0_dp / sqrt(3.0_dp), 0.0_dp, dp)
  psi_big = psi
  A_mu = 0.0_dp
  b = [Delta, 0.0_dp, 0.0_dp]

  ! ハミルトニアン初期化
  H0 = cmplx(0.0_dp, 0.0_dp, dp)
  H_BIG = cmplx(0.0_dp, 0.0_dp, dp)
  do i = 1, n_dim
    H0(i,i) = cmplx(w(i), 0.0_dp, dp)
  end do
  H_BIG(1,1) = cmplx(Delta, 0.0_dp, dp)

  ! シミュレーションループ
  t = 0.0_dp
  do i = 1, n_steps
    ! 古典的ダイナミクス
    call update_classical(x, A_mu, b, .false., x_new)
    x = x_new
    call update_classical(x_big, A_mu, b, .true., x_new)
    x_big = x_new

    ! 量子ダイナミクス
    call update_quantum(psi, H0, H_BIG, .false., psi_new)
    psi = psi_new
    call update_quantum(psi_big, H0, H_BIG, .true., psi_new)
    psi_big = psi_new

    ! 幸福感計算
    H_classical(i) = happiness_classical(x)
    H_classical_big(i) = happiness_classical(x_big)
    H_quantum(i) = real(happiness_quantum(psi, H0))
    H_quantum_big(i) = real(happiness_quantum(psi_big, H0))
    H_total(i) = lambda * H_classical(i) + (1.0_dp - lambda) * H_quantum(i)
    H_total_big(i) = lambda * H_classical_big(i) + (1.0_dp - lambda) * H_quantum_big(i)

    t = t + dt
  end do

  ! 結果集約（マスタープロセスのみ出力）
  if (rank == 0) then
    open(unit=10, file='happiness_gauge_simulation.dat', status='replace')
    do i = 1, n_steps
      write(10, '(3F12.6)') (i-1)*dt, H_total(i), H_total_big(i)
    end do
    close(10)
  end if

  ! MPI終了
  call MPI_FINALIZE(ierr)

contains

  ! 古典的遷移行列
  subroutine compute_A(x, A_mu, A_mat)
    real(dp), intent(in) :: x(n_dim), A_mu(n_dim)
    real(dp), intent(out) :: A_mat(n_dim, n_dim)
    real(dp) :: F(n_dim, n_dim)
    integer :: i, j

    A_mat = 0.0_dp
    F = 0.0_dp  ! 簡略化した場強度
    do i = 1, n_dim
      do j = 1, n_dim
        A_mat(i,j) = 0.1_dp * real(i, dp) * real(j, dp) * (1.0_dp / (1.0_dp + exp(-x(j)))) + g * F(i,j)
      end do
    end do
    A_mat(1,1) = 0.8_dp
    A_mat(2,2) = 0.7_dp
    A_mat(3,3) = 0.6_dp
  end subroutine compute_A

  ! 古典的ダイナミクス更新
  subroutine update_classical(x, A_mu, b, use_big, x_new)
    real(dp), intent(in) :: x(n_dim), A_mu(n_dim), b(n_dim)
    logical, intent(in) :: use_big
    real(dp), intent(out) :: x_new(n_dim)
    real(dp) :: A_mat(n_dim, n_dim), A_mu_eff(n_dim), temp(n_dim)
    integer :: i

    A_mu_eff = A_mu
    if (use_big) A_mu_eff = A_mu + b
    call compute_A(x, A_mu_eff, A_mat)

    ! A_mat * x
    call dgemv('N', n_dim, n_dim, 1.0_dp, A_mat, n_dim, x, 1, 0.0_dp, temp, 1)
    x_new = temp

    ! BIG外力とノイズ
    if (use_big) x_new = x_new + b
    call random_number(rand_vec)
    rand_vec = rand_vec - 0.5_dp
    x_new = x_new + sigma * rand_vec
  end subroutine update_classical

  ! 量子ダイナミクス更新
  subroutine update_quantum(psi, H0, H_BIG, use_big, psi_new)
    complex(dp), intent(in) :: psi(n_dim), H0(n_dim, n_dim), H_BIG(n_dim, n_dim)
    logical, intent(in) :: use_big
    complex(dp), intent(out) :: psi_new(n_dim)
    complex(dp) :: H(n_dim, n_dim), U(n_dim, n_dim), temp(n_dim)
    complex(dp) :: I_cmplx = cmplx(0.0_dp, 1.0_dp, dp)
    integer :: i, j

    H = H0
    if (use_big) H = H + H_BIG

    ! 簡略化したユニタリ演算子: U = exp(-i H dt / hbar) ≈ I - i H dt / hbar
    U = cmplx(0.0_dp, 0.0_dp, dp)
    do i = 1, n_dim
      U(i,i) = cmplx(1.0_dp, 0.0_dp, dp)
    end do
    U = U - I_cmplx * H * (dt / hbar)

    ! psi_new = U * psi
    call zgemv('N', n_dim, n_dim, cmplx(1.0_dp, 0.0_dp, dp), U, n_dim, psi, 1, cmplx(0.0_dp, 0.0_dp, dp), temp, 1)
    psi_new = temp

    ! 正規化
    psi_new = psi_new / sqrt(sum(abs(psi_new)**2))
  end subroutine update_quantum

  ! 古典的幸福感
  real(dp) function happiness_classical(x)
    real(dp), intent(in) :: x(n_dim)
    happiness_classical = 1.0_dp / (1.0_dp + exp(-dot_product(w, x)))
  end function happiness_classical

  ! 量子幸福感
  complex(dp) function happiness_quantum(psi, H_op)
    complex(dp), intent(in) :: psi(n_dim), H_op(n_dim, n_dim)
    complex(dp) :: temp(n_dim)
    call zgemv('N', n_dim, n_dim, cmplx(1.0_dp, 0.0_dp, dp), H_op, n_dim, psi, 1, cmplx(0.0_dp, 0.0_dp, dp), temp, 1)
    happiness_quantum = dot_product(psi, temp)
  end function happiness_quantum

end program happiness_simulation
