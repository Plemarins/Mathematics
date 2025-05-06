module theorems
  implicit none
  complex, parameter :: large_s = cmplx(1.0e6, 0.0)
  complex, parameter :: small_s = cmplx(1.0e-6, 0.0)
  real, parameter :: tolerance = 1.0e-10
contains

  ! Laplace transform F(s) = 1/(s^2 + 2s + 1)
  complex function laplace_f(s)
    complex, intent(in) :: s
    laplace_f = 1.0 / (s * s + 2.0 * s + 1.0)
  end function laplace_f

  ! Initial Value Theorem: lim_{s->infty} s*F(s)
  complex function initial_value()
    initial_value = large_s * laplace_f(large_s)
  end function initial_value

  ! Final Value Theorem: lim_{s->0} s*F(s)
  complex function final_value()
    final_value = small_s * laplace_f(small_s)
  end function final_value

  ! Stability Analysis: Check if all poles have negative real parts
  logical function is_stable(poles, n)
    integer, intent(in) :: n
    complex, intent(in) :: poles(n)
    integer :: i
    is_stable = .true.
    do i = 1, n
      if (real(poles(i)) >= 0.0) then
        is_stable = .false.
        exit
      end if
    end do
  end function is_stable

  ! Tellegen's Theorem: Sum of v_k * i_k
  complex function tellegen_sum(voltages, currents, n)
    integer, intent(in) :: n
    complex, intent(in) :: voltages(n), currents(n)
    integer :: k
    tellegen_sum = cmplx(0.0, 0.0)
    do k = 1, n
      tellegen_sum = tellegen_sum + voltages(k) * currents(k)
    end do
  end function tellegen_sum

  ! Verify Tellegen's Theorem
  logical function verify_tellegen(sum)
    complex, intent(in) :: sum
    verify_tellegen = abs(sum) < tolerance
  end function verify_tellegen

  ! Adjacency Matrix for Circuit Graph
  subroutine adjacency_matrix(edges, n_edges, n_nodes, matrix)
    integer, intent(in) :: n_edges, n_nodes
    integer, intent(in) :: edges(n_edges, 2)
    integer, intent(out) :: matrix(n_nodes, n_nodes)
    integer :: i
    matrix = 0
    do i = 1, n_edges
      matrix(edges(i, 1), edges(i, 2)) = 1
      matrix(edges(i, 2), edges(i, 1)) = 1 ! Undirected graph
    end do
  end subroutine adjacency_matrix

end module theorems

program main
  use theorems
  implicit none
  complex :: init_val, fin_val, tel_sum
  complex :: voltages(2), currents(2)
  complex :: poles(2)
  integer :: edges(2, 2), adj_matrix(2, 2)
  logical :: tel_verified, stable
  integer :: i, j

  ! Initialize data
  voltages = [cmplx(2.0, 0.0), cmplx(-2.0, 0.0)]
  currents = [cmplx(3.0, 0.0), cmplx(-3.0, 0.0)]
  poles = [cmplx(-1.0, 0.0), cmplx(-1.0, 0.0)] ​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​
