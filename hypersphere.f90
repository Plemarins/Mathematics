program hypersphere
  implicit none

  ! 定数
  integer, parameter :: n_dim = 4           ! S^3 (4次元)
  integer, parameter :: n_nodes = 40        ! ノード数
  integer, parameter :: k_neighbors = 6     ! k近傍
  integer, parameter :: n_steps = 20        ! コンセンサスステップ数
  real(8), parameter :: pi = 3.141592653589793d0
  real(8), parameter :: comm_base_prob = 0.5d0 ! 基本通信確率

  ! 動的配列
  real(8), allocatable :: nodes(:, :)       ! ノード座標 (n_nodes, n_dim)
  real(8), allocatable :: states(:)         ! ノード状態
  integer, allocatable :: edges(:, :)       ! エッジ (from, to)
  real(8), allocatable :: weights(:)        ! エッジ重み
  real(8), allocatable :: voronoi_areas(:)  ! Voronoi面積（近似）
  integer :: n_edges                        ! エッジ数

  ! 作業変数
  integer :: i, j, step
  real(8) :: conv_error, load_balance_error
  real(8) :: avg_degree, avg_voronoi_area

  ! 初期化
  call random_seed()
  allocate(nodes(n_nodes, n_dim))
  allocate(states(n_nodes))
  allocate(voronoi_areas(n_nodes))

  ! ノード生成と初期状態設定
  call generate_nodes(nodes, states)

  ! エッジ生成（k近傍グラフ）
  call generate_edges(nodes, edges, weights, n_edges)

  ! トポロジー解析
  call analyze_topology(nodes, edges, weights, n_edges, voronoi_areas, &
                        avg_degree, avg_voronoi_area)

  ! 出力：トポロジー指標
  print *, 'Topology Metrics (S^3):'
  print '(A, F6.2)', 'Average Degree: ', avg_degree
  print '(A, F6.3)', 'Average Voronoi Area (approx): ', avg_voronoi_area

  ! 出力：Voronoi面積
  print *, ''
  print *, 'Voronoi Areas (approx):'
  do i = 1, n_nodes
    print '(A, I2, A, F6.3)', 'Node ', i-1, ': area = ', voronoi_areas(i)
  end do

  ! 初期状態出力
  print *, ''
  print *, 'Initial States:'
  do i = 1, n_nodes
    print '(A, I2, A, F6.3)', 'Node ', i-1, ': state = ', states(i)
  end do

  ! 非同期コンセンサス
  print *, ''
  print '(A, I0, A)', 'Simulating Async Consensus with Voronoi (', n_steps, ' steps)...'
  do step = 1, n_steps
    call update_states_async(nodes, edges, weights, n_edges, states, voronoi_areas)
  end do

  ! 最終状態出力
  print *, ''
  print *, 'Final States (Consensus):'
  do i = 1, n_nodes
    print '(A, I2, A, F6.3)', 'Node ', i-1, ': state = ', states(i)
  end do

  ! 収束誤差
  call compute_convergence_error(states, conv_error)
  print '(A, F8.5)', 'Convergence Error: ', conv_error

  ! 負荷バランス
  call compute_load_balance(voronoi_areas, load_balance_error)
  print '(A, F6.3)', 'Load Balance Error: ', load_balance_error

  ! メモリ解放
  deallocate(nodes, states, edges, weights, voronoi_areas)

contains

  ! ノード生成
  subroutine generate_nodes(nodes, states)
    real(8), intent(out) :: nodes(:, :), states(:)
    real(8) :: coords(n_dim), norm
    integer :: i, j
    do i = 1, n_nodes
      ! ガウス分布で座標生成
      do j = 1, n_dim
        call random_normal(coords(j))
      end do
      ! 正規化して S^3 上に
      norm = sqrt(sum(coords**2))
      nodes(i, :) = coords / norm
      ! 初期状態（0～10のランダム）
      call random_number(states(i))
      states(i) = states(i) * 10.0d0
    end do
  end subroutine generate_nodes

  ! ガウス乱数生成（Box-Muller法）
  subroutine random_normal(x)
    real(8), intent(out) :: x
    real(8) :: u1, u2
    call random_number(u1)
    call random_number(u2)
    x = sqrt(-2.0d0 * log(u1)) * cos(2.0d0 * pi * u2)
  end subroutine random_normal

  ! 大円距離
  function great_circle_distance(p1, p2) result(dist)
    real(8), intent(in) :: p1(:), p2(:)
    real(8) :: dist, dot
    dot = sum(p1 * p2)
    dist = acos(max(-1.0d0, min(1.0d0, dot)))
  end function great_circle_distance

  ! エッジ生成（k近傍グラフ）
  subroutine generate_edges(nodes, edges, weights, n_edges)
    real(8), intent(in) :: nodes(:, :)
    integer, allocatable, intent(out) :: edges(:, :)
    real(8), allocatable, intent(out) :: weights(:)
    integer, intent(out) :: n_edges
    real(8) :: distances(n_nodes)
    integer :: indices(n_nodes), i, j, k, count
    real(8) :: dist
    ! 一時配列
    integer :: temp_edges(n_nodes * k_neighbors, 2)
    real(8) :: temp_weights(n_nodes * k_neighbors)
    count = 0
    do i = 1, n_nodes
      ! 全ノードとの距離を計算
      do j = 1, n_nodes
        if (i == j) then
          distances(j) = huge(1.0d0)
        else
          distances(j) = great_circle_distance(nodes(i, :), nodes(j, :))
        end if
        indices(j) = j
      end do
      ! 距離でソート（簡略化のためバブルソート）
      do j = 1, n_nodes-1
        do k = j+1, n_nodes
          if (distances(j) > distances(k)) then
            dist = distances(j)
            distances(j) = distances(k)
            distances(k) = dist
            k = indices(j)
            indices(j) = indices(k)
            indices(k) = k
          end if
        end do
      end do
      ! k近傍を選択
      do j = 1, k_neighbors
        if (indices(j) /= i) then
          count = count + 1
          temp_edges(count, 1) = i
          temp_edges(count, 2) = indices(j)
          temp_weights(count) = distances(j)
        end if
      end do
    end do
    ! 双方向エッジを追加
    n_edges = count * 2
    allocate(edges(n_edges, 2), weights(n_edges))
    do i = 1, count
      edges(i, 1) = temp_edges(i, 1)
      edges(i, 2) = temp_edges(i, 2)
      weights(i) = temp_weights(i)
      edges(count+i, 1) = temp_edges(i, 2)
      edges(count+i, 2) = temp_edges(i, 1)
      weights(count+i) = temp_weights(i)
    end do
  end subroutine generate_edges

  ! トポロジー解析
  subroutine analyze_topology(nodes, edges, weights, n_edges, voronoi_areas, &
                             avg_degree, avg_voronoi_area)
    real(8), intent(in) :: nodes(:, :), weights(:)
    integer, intent(in) :: edges(:, :), n_edges
    real(8), intent(out) :: voronoi_areas(:), avg_degree, avg_voronoi_area
    integer :: i, j, node_id, count
    real(8) :: sum_dist
    ! 平均次数
    avg_degree = real(n_edges, 8) / real(n_nodes, 8)
    ! Voronoi面積（近隣距離の逆数）
    do i = 1, n_nodes
      sum_dist = 0.0d0
      count = 0
      do j = 1, n_edges
        if (edges(j, 1) == i) then
          sum_dist = sum_dist + weights(j)
          count = count + 1
        end if
      end do
      if (count > 0) then
        voronoi_areas(i) = 1.0d0 / (sum_dist / real(count, 8))
      else
        voronoi_areas(i) = 0.0d0
      end if
    end do
    avg_voronoi_area = sum(voronoi_areas) / real(n_nodes, 8)
  end subroutine analyze_topology

  ! 非同期コンセンサス
  subroutine update_states_async(nodes, edges, weights, n_edges, states, voronoi_areas)
    real(8), intent(in) :: nodes(:, :), weights(:), voronoi_areas(:)
    integer, intent(in) :: edges(:, :), n_edges
    real(8), intent(inout) :: states(:)
    real(8) :: new_states(n_nodes), max_area, comm_prob, r
    integer :: i, j
    new_states = states
    max_area = maxval(voronoi_areas)
    do i = 1, n_nodes
      ! 面積に基づく通信確率
      comm_prob = comm_base_prob * (1.0d0 - voronoi_areas(i) / max_area)
      call random_number(r)
      if (r < comm_prob) then
        ! 近隣の加重平均
        real(8) :: weighted_sum, weight_sum
        integer :: count
        weighted_sum = 0.0d0
        weight_sum = 0.0d0
        count = 0
        do j = 1, n_edges
          if (edges(j, 1) == i) then
            weighted_sum = weighted_sum + states(edges(j, 2)) / weights(j)
            weight_sum = weight_sum + 1.0d0 / weights(j)
            count = count + 1
          end if
        end do
        if (count > 0) then
          new_states(i) = weighted_sum / weight_sum
        end if
      end if
    end do
    states = new_states
  end subroutine update_states_async

  ! 収束誤差
  subroutine compute_convergence_error(states, conv_error)
    real(8), intent(in) :: states(:)
    real(8), intent(out) :: conv_error
    real(8) :: avg_state
    avg_state = sum(states) / real(n_nodes, 8)
    conv_error = sum(abs(states - avg_state)) / real(n_nodes, 8)
  end subroutine compute_convergence_error

  ! 負荷バランス
  subroutine compute_load_balance(voronoi_areas, load_balance_error)
    real(8), intent(in) :: voronoi_areas(:)
    real(8), intent(out) :: load_balance_error
    integer :: comm_counts(n_nodes), i, j, count
    real(8) :: max_area, comm_prob, r, avg_count
    comm_counts = 0
    max_area = maxval(voronoi_areas)
    do j = 1, 100
      do i = 1, n_nodes
        comm_prob = comm_base_prob * (1.0d0 - voronoi_areas(i) / max_area)
        call random_number(r)
        if (r < comm_prob) then
          comm_counts(i) = comm_counts(i) + 1
        end if
      end do
    end do
    avg_count = real(sum(comm_counts), 8) / real(n_nodes, 8)
    load_balance_error = sum(abs(comm_counts - avg_count)) / real(n_nodes, 8)
  end subroutine compute_load_balance

end program hypersphere
