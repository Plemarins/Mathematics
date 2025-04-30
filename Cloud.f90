! クラウドAPIセキュリティモデル: 認証と認可のシミュレーション
! 多層: クライアントがサーバーにリクエストを送信
! 多要素: APIキー認証、認可チェック
! 多次元: リソースごとのアクセス権限管理
program cloud_api_security
    implicit none
    
    ! リソース構造体
    type :: Resource
        character(len=20) :: id
        character(len=20) :: owner
        character(len=20) :: status
    end type Resource
    
    ! グローバル変数
    type(Resource), dimension(10) :: resources
    integer :: resource_count = 0
    character(len=20) :: api_key = "valid_key"
    
    ! プロトタイプ宣言
    logical :: authenticate
    logical :: authorize
    logical :: create_resource
    logical :: get_resource
    
    ! テスト実行
    call test_api
    
contains

    ! 認証関数
    logical function authenticate(key)
        character(len=*), intent(in) :: key
        if (key == api_key) then
            authenticate = .true.
            print *, "Authentication successful"
        else
            authenticate = .false.
            print *, "Authentication failed"
        end if
    end function authenticate
    
    ! 認可関数
    logical function authorize(key, owner)
        character(len=*), intent(in) :: key, owner
        ! 簡略化: APIキーが正しければ所有者として認可
        if (key == api_key .and. owner == "user1") then
            authorize = .true.
            print *, "Authorization successful for ", owner
        else
            authorize = .false.
            print *, "Authorization failed for ", owner
        end if
    end function authorize
    
    ! リソース作成関数
    logical function create_resource(key, id, owner)
        character(len=*), intent(in) :: key, id, owner
        if (.not. authenticate(key)) then
            create_resource = .false.
            return
        end if
        if (.not. authorize(key, owner)) then
            create_resource = .false.
            return
        end if
        if (resource_count >= 10) then
            print *, "Resource limit reached"
            create_resource = .false.
            return
        end if
        resource_count = resource_count + 1
        resources(resource_count)%id = id
        resources(resource_count)%owner = owner
        resources(resource_count)%status = "RUNNING"
        print *, "Resource created: ", id, " by ", owner
        create_resource = .true.
    end function create_resource
    
    ! リソース取得関数
    logical function get_resource(key, id, owner, status)
        character(len=*), intent(in) :: key, id, owner
        character(len=20), intent(out) :: status
        integer :: i
        if (.not. authenticate(key)) then
            get_resource = .false.
            return
        end if
        if (.not. authorize(key, owner)) then
            get_resource = .false.
            return
        end if
        do i = 1, resource_count
            if (resources(i)%id == id .and. resources(i)%owner == owner) then
                status = resources(i)%status
                print *, "Resource found: ", id, " Status: ", status
                get_resource = .true.
                return
            end if
        end do
        print *, "Resource not found or unauthorized: ", id
        get_resource = .false.
    end function get_resource
    
    ! テスト関数
    subroutine test_api
        character(len=20) :: status
        logical :: success
        
        ! リソース作成テスト
        success = create_resource("valid_key", "vm1", "user1")
        success = create_resource("invalid_key", "vm2", "user1")
        success = create_resource("valid_key", "vm3", "user2")
        
        ! リソース取得テスト
        success = get_resource("valid_key", "vm1", "user1", status)
        success = get_resource("valid_key", "vm1", "user2", status)
    end subroutine test_api

end program cloud_api_security
