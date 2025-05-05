program EuclideanGeometry
    implicit none
    
    ! 型定義：点
    type :: Point
        real :: x, y
    end type Point
    
    ! 型定義：線（2点で定義）
    type :: Line
        type(Point) :: p1, p2
    end type Line
    
    ! プロトタイプ宣言
    real :: distance
    logical :: is_parallel
    type(Point) :: translate, rotate
    real, parameter :: PI = 3.141592653589793
    
    ! テスト用変数
    type(Point) :: p1, p2, p3, p4
    type(Line) :: l1, l2
    type(Point) :: p_trans, p_rot
    real :: dist
    
    ! 点の初期化
    p1 = Point(0.0, 0.0)
    p2 = Point(1.0, 1.0)
    p3 = Point(0.0, 2.0)
    p4 = Point(1.0, 3.0)
    
    ! 線の初期化
    l1 = Line(p1, p2)
    l2 = Line(p3, p4)
    
    ! 距離計算
    dist = distance(p1, p2)
    print *, 'Distance between p1 and p2: ', dist
    
    ! 平行性判定
    if (is_parallel(l1, l2)) then
        print *, 'Lines are parallel'
    else
        print *, 'Lines are not parallel'
    end if
    
    ! 平行移動
    p_trans = translate(p1, 2.0, 3.0)
    print *, 'Translated point: (', p_trans%x, ',', p_trans%y, ')'
    
    ! 回転
    p_rot = rotate(p1, PI/2.0)
    print *, 'Rotated point: (', p_rot%x, ',', p_rot%y, ')'

contains

    ! 距離関数（体論：ユークリッド距離）
    real function distance(p, q)
        type(Point), intent(in) :: p, q
        distance = sqrt((p%x - q%x)**2 + (p%y - q%y)**2)
    end function distance
    
    ! 平行性判定（群論的構造の簡略化）
    logical function is_parallel(l1, l2)
        type(Line), intent(in) :: l1, l2
        real :: slope1, slope2
        real, parameter :: EPS = 1.0e-10
        
        ! 傾きの計算（ゼロ除算回避）
        slope1 = (l1%p2%y - l1%p1%y) / (l1%p2%x - l1%p1%x + EPS)
        slope2 = (l2%p2%y - l2%p1%y) / (l2%p2%x - l2%p1%x + EPS)
        
        ! 傾きが等しいか判定
        is_parallel = abs(slope1 - slope2) < EPS
    end function is_parallel
    
    ! 平行移動（群論：ユークリッド群の作用）
    type(Point) function translate(p, dx, dy)
        type(Point), intent(in) :: p
        real, intent(in) :: dx, dy
        translate = Point(p%x + dx, p%y + dy)
    end function translate
    
    ! 回転（群論：直交群 O(2)）
    type(Point) function rotate(p, angle)
        type(Point), intent(in) :: p
        real, intent(in) :: angle
        real :: cos_a, sin_a
        cos_a = cos(angle)
        sin_a = sin(angle)
        rotate = Point(p%x * cos_a - p%y * sin_a, p%x * sin_a + p%y * cos_a)
    end function rotate

end program EuclideanGeometry
