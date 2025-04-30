! ディレクテッドエボリューションの拡張シミュレーション
! アナロジー：鍵（シーケンス）を変形して錠（ターゲット）に最適化
! 論理：点変異→適応度評価→選択を繰り返し、多様性を維持
program directed_evolution
    implicit none
    ! パラメータ
    integer, parameter :: n_pop = 100        ! 集団サイズ
    integer, parameter :: n_gen = 50         ! 最大世代数
    integer, parameter :: seq_len = 10       ! シーケンス長
    real, parameter :: fitness_threshold = 0.9 ! 適応度閾値
    real, parameter :: mutation_rate = 1.0   ! 変異率（1シーケンスあたり1箇所）
    character :: amino_acids(4) = ['A', 'C', 'G', 'T'] ! アミノ酸（簡略化）
    character :: target(seq_len)             ! ターゲットシーケンス
    character :: pop(n_pop, seq_len)         ! シーケンス集団
    character :: mutants(n_pop, seq_len)     ! 変異体
    real :: fitnesses(n_pop)                 ! 適応度
    real :: fitness_history(n_gen, 2)        ! 最大・平均適応度の履歴
    integer :: sorted_idx(n_pop)             ! ソートインデックス
    integer :: gen, i, pos, aa_idx
    real :: r, avg_diversity

    ! ターゲットシーケンス：AAAAAACCCC
    target(1:6) = 'A'
    target(7:10) = 'C'

    ! 初期化：ランダムなシーケンス集団
    call random_seed()
    do i = 1, n_pop
        do pos = 1, seq_len
            call random_number(r)
            pop(i, pos) = amino_acids(int(r * 4) + 1)
        end do
    end do

    ! メインループ
    do gen = 1, n_gen
        ! 変異：1箇所の点変異
        mutants = pop
        do i = 1, n_pop
            call random_number(r)
            if (r < mutation_rate) then
                call random_number(r)
                pos = int(r * seq_len) + 1
                call random_number(r)
                aa_idx = int(r * 4) + 1
                mutants(i, pos) = amino_acids(aa_idx)
            end if
        end do

        ! 評価：適応度（ハミング距離の逆数）
        do i = 1, n_pop
            fitnesses(i) = compute_fitness(mutants(i,:), target)
        end do

        ! 多様性計算：集団の平均ハミング距離
        avg_diversity = compute_diversity(mutants, n_pop, seq_len)

        ! 選択：上位50%を保持、残りはランダム生成
        call sort(fitnesses, sorted_idx, n_pop)
        do i = 1, n_pop/2
            pop(i,:) = mutants(sorted_idx(i),:)
        end do
        do i = n_pop/2 + 1, n_pop
            do pos = 1, seq_len
                call random_number(r)
                pop(i, pos) = amino_acids(int(r * 4) + 1)
            end do
        end do

        ! 適応度履歴の記録
        fitness_history(gen, 1) = maxval(fitnesses) ! 最大適応度
        fitness_history(gen, 2) = sum(fitnesses) / n_pop ! 平均適応度

        ! 結果表示
        print '(A,I3,A,F6.4,A,F6.4,A,F6.4)', &
              'Gen ', gen, ': Best fitness = ', fitness_history(gen, 1), &
              ', Avg fitness = ', fitness_history(gen, 2), &
              ', Diversity = ', avg_diversity
        print '(A,10A1)', 'Best seq = ', (mutants(sorted_idx(1),i),i=1,seq_len)

        ! 終了条件：適応度閾値に達した場合
        if (fitness_history(gen, 1) >= fitness_threshold) then
            print *, 'Target fitness reached. Stopping.'
            exit
        end if
    end do

    ! 最終結果：適応度履歴の要約
    print *, 'Evolution Summary:'
    print *, 'Generation | Max Fitness | Avg Fitness'
    do gen = 1, min(n_gen, gen) ! 最後の世代まで
        print '(I10,2F12.4)', gen, fitness_history(gen, 1), fitness_history(gen, 2)
    end do

contains
    ! 適応度関数：ハミング距離の逆数
    real function compute_fitness(seq, target)
        character, intent(in) :: seq(seq_len), target(seq_len)
        integer :: hamming_distance
        hamming_distance = sum(merge(1, 0, seq /= target))
        compute_fitness = 1.0 / (1.0 + real(hamming_distance))
    end function compute_fitness

    ! 多様性計算：集団内の平均ハミング距離
    real function compute_diversity(pop, n_pop, seq_len)
        character, intent(in) :: pop(n_pop, seq_len)
        integer, intent(in) :: n_pop, seq_len
        integer :: i, j, hamming_sum
        real :: pairs
        hamming_sum = 0
        pairs = 0.0
        do i = 1, n_pop-1
            do j = i+1, n_pop
                hamming_sum = hamming_sum + sum(merge(1, 0, pop(i,:) /= pop(j,:)))
                pairs = pairs + 1.0
            end do
        end do
        compute_diversity = real(hamming_sum) / pairs / real(seq_len)
    end function compute_diversity

    ! ソート（簡略化：実際はバブルソートなど実装）
    subroutine sort(arr, idx, n)
        real, intent(in) :: arr(n)
        integer, intent(out) :: idx(n)
        integer, intent(in) :: n
        real :: temp_arr(n)
        integer :: i, j, temp_idx
        temp_arr = arr
        do i = 1, n
            idx(i) = i
        end do
        ! バブルソート（降順）
        do i = 1, n-1
            do j = 1, n-i
                if (temp_arr(j) < temp_arr(j+1)) then
                    temp_arr(j:j+1) = temp_arr(j+1:j:-1)
                    temp_idx = idx(j)
                    idx(j) = idx(j+1)
                    idx(j+1) = temp_idx
                end if
            end do
        end do
    end subroutine sort
end program directed_evolution
