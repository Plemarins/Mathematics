program gene_editing_simulation
  use mpi
  implicit none
  integer :: i, j, num_cells, num_loci, ierr, rank, size
  real :: marker_level, edit_prob, fitness
  real, allocatable :: genome(:,:), fitness_array(:)
  integer :: seed

  ! MPI初期化
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

  ! パラメータ設定
  num_cells = 1000
  num_loci = 10
  allocate(genome(num_cells, num_loci))
  allocate(fitness_array(num_cells))
  seed = 12345 + rank

  ! 乱数初期化
  call srand(seed)

  ! 初期集団生成
  do i = 1, num_cells
    marker_level = rand() ! 細胞表面マーカー
    do j = 1, num_loci
      genome(i,j) = rand() ! 遺伝子型
    end do
  end do

  ! 遺伝子編集（CRISPR/TALEN）
  do i = 1, num_cells
    edit_prob = 0.8 * (1.0 - genome(i,1)) ! ヌクレアーゼ耐性を考慮
    if (rand() < edit_prob) then
      genome(i,1) = 1.0 ! 編集成功
    end if
  end do

  ! 適応度計算（マルチローカス選択）
  do i = 1, num_cells
    fitness = 0.0
    do j = 1, num_loci
      fitness = fitness + genome(i,j)
    end do
    fitness_array(i) = fitness / num_loci
  end do

  ! 結果集約
  call MPI_REDUCE(fitness_array, fitness_array, num_cells, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  ! 出力（ランク0のみ）
  if (rank == 0) then
    print *, 'Average fitness:', sum(fitness_array) / num_cells
  end if

  ! 終了
  deallocate(genome, fitness_array)
  call MPI_FINALIZE(ierr)
end program
