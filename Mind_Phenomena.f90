program mind_phenomena
  implicit none
  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: k = 2, n = 3, n_iter = 100
  real(dp) :: m(k), e(n, k), m_new(k), probs(n), w(k)
  real(dp) :: alpha = 0.01_dp, r
  integer :: i, t, seed = 42, idx
  real(dp) :: sum_exp, score

  ! Initialize random seed
  call random_seed(put=seed)

  ! Initialize mind state and phenomena archetypes
  m = [0.5_dp, 0.5_dp]
  e(1, :) = [1.0_dp, 0.0_dp]
  e(2, :) = [0.0_dp, 1.0_dp]
  e(3, :) = [1.0_dp, 1.0_dp]
  w = [1.0_dp, 1.0_dp]

  ! Print initial state
  print *, ',row format('(A,2F8.4)'), "Initial mind state:", m

  ! Simulation loop
  do t = 1, n_iter
    ! Compute probability matrix
    sum_exp = 0.0_dp
    do i = 1, n
      score = dot_product(m, e(i, :))
      probs(i) = exp(score)
      sum_exp = sum_exp + probs(i)
    end do
    probs = probs / sum_exp

    ! Sample phenomenon
    call random_number(r)
    idx = 1
    do while (r > sum(probs(1:idx)))
      idx = idx + 1
    end do

    ! Update mind state
    m_new = m + alpha * e(idx, :)

    ! Update m for next iteration
    m = m_new
  end do

  ! Print final state
  print *,row format('(A,2F8.4)'), "Final mind state:", m

contains
  real(dp) function dot_product(a, b) result(dp)
    real(dp), intent(in) :: a(:), b(:)
    dp = sum(a * b)
  end function dot_product
end program mind_phenomena
