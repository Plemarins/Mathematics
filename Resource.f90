program ResourceEvolution
    implicit none

    ! 集合論：状態空間と戦略空間
    integer, parameter :: n_strategies = 2  ! 戦略数（Cooperative, Defective）
    real(8), dimension(n_strategies) :: strategy_distribution  ! 戦略分布 x
    real(8) :: resource  ! リソース量 r
    real(8) :: fitness  ! 平均適応度 \bar{f}
    real(8), dimension(n_strategies) :: strategy_costs = (/0.3, 0.1/)  ! 戦略のコスト（環論）
    real(8), parameter :: max_resource = 10.0  ! 最大リソース量
    real(8), parameter :: fitness_threshold = 1.0  ! 適応度閾値
    real(8), parameter :: resource_decay = 0.1  ! リソース消費率
    real(8), parameter :: dt = 0.01  ! 時間ステップ（確率論）
    integer, parameter :: max_steps = 1000  ! 最大ステップ数

    ! 作業変数
    integer :: i, step
    real(8) :: final_fitness, final_resource
    logical :: high_fitness_reached

    ! 初期状態（集合論：状態空間の初期化）
    call initialize_state(strategy_distribution, resource, fitness)

    ! シミュレーション（圏論：状態遷移）
    call simulate_evolution(strategy_distribution, resource, fitness, &
                            final_fitness, final_resource, high_fitness_reached)

    ! 結果出力（モーダル論理：\diamond Fitness(s^*, r')）
    print *, 'Final Fitness:', final_fitness
    print *, 'Final Resource:', final_resource
    if (high_fitness_reached) then
        print *, 'Evolved to high fitness'
    else
        print *, 'Did not evolve to high fitness'
    end if

contains

    ! 初期状態の設定
    subroutine initialize_state(dist, res, fit)
        real(8), dimension(:), intent(out) :: dist
        real(8), intent(out) :: res, fit
        real(8) :: sum_dist

        ! 初期戦略分布（例：均等）
        dist(1) = 0.5  ! Cooperative
        dist(2) = 0.5  ! Defective
        sum_dist = sum(dist)
        dist = dist / sum_dist  ! 正規化（群論：対称性）

        ! 初期リソース（豊富）
        res = max_resource

        ! 初期適応度
        fit = 0.0
    end subroutine initialize_state

    ! 適応度計算（環論：実数環での評価）
    function compute_fitness(strategy_idx, res) result(f)
        integer, intent(in) :: strategy_idx
        real(8), intent(in) :: res
        real(8) :: f
        ! f(\sigma, r) \propto r / (1 + c(\sigma)) （体論：一貫性）
        f = res / (1.0 + strategy_costs(strategy_idx))
    end function compute_fitness

    ! 平均適応度計算
    function compute_average_fitness(dist, res) result(avg_f)
        real(8), dimension(:), intent(in) :: dist
        real(8), intent(in) :: res
        real(8) :: avg_f
        integer :: i
        avg_f = 0.0
        do i = 1, n_strategies
            avg_f = avg_f + dist(i) * compute_fitness(i, res)
        end do
    end function compute_average_fitness

    ! レプリケーターダイナミクス（確率論：レプリケーターダイナミクス）
    subroutine update_strategy_distribution(dist, res, fit)
        real(8), dimension(:), intent(inout) :: dist
        real(8), intent(in) :: res
        real(8), intent(out) :: fit
        real(8), dimension(n_strategies) :: new_dist
        real(8) :: avg_f, sum_dist
        integer :: i

        avg_f = compute_average_fitness(dist, res)
        sum_dist = 0.0
        do i = 1, n_strategies
            ! \dot{x}_\sigma = x_\sigma (f(\sigma, r) - \bar{f})
            new_dist(i) = dist(i) * (compute_fitness(i, res) - avg_f) * dt + dist(i)
            if (new_dist(i) < 0.0) new_dist(i) = 0.0
            sum_dist = sum_dist + new_dist(i)
        end do

        ! 正規化（群論：戦略分布の対称性）
        if (sum_dist > 0.0) then
            new_dist = new_dist / sum_dist
        end if
        dist = new_dist
        fit = compute_average_fitness(dist, res)
    end subroutine update_strategy_distribution

    ! 進化シミュレーション（圏論：状態遷移プロセス）
    subroutine simulate_evolution(dist, res, fit, final_fit, final_res, reached)
        real(8), dimension(:), intent(inout) :: dist
        real(8), intent(inout) :: res
        real(8), intent(inout) :: fit
        real(8), intent(out) :: final_fit, final_res
        logical, intent(out) :: reached
        real(8) :: avg_cost
        integer :: step

        final_fit = fit
        final_res = res
        reached = .false.

        do step = 1, max_steps
            if (res <= 0.0) exit

            ! リソース消費（環論：リソースの減法）
            avg_cost = sum(dist * strategy_costs)
            res = res - resource_decay * avg_cost
            if (res < 0.0) res = 0.0

            ! 戦略分布更新（確率論）
            call update_strategy_distribution(dist, res, fit)

            ! 適応度チェック（数理論理：HighFitness）
            if (fit >= fitness_threshold) then
                reached = .true.
                exit
            end if
        end do

        final_fit = fit
        final_res = res
    end subroutine simulate_evolution

end program ResourceEvolution
