! モーダル論理の公理系（ML-MOD）をFORTRANで実装
! 圏論・代数的解釈（圏、群、環、体）を反映
! 公理と推論規則を関数として定義し、可能世界フレームをシミュレーション
PROGRAM ModalLogic
  IMPLICIT NONE

  ! モジュール定義：モーダル論理の構造と関数
  MODULE ModalLogicModule
    IMPLICIT NONE
    INTEGER, PARAMETER :: MAX_WORLDS = 100 ! 最大可能世界数
    INTEGER, PARAMETER :: MAX_PROPOSITIONS = 50 ! 最大命題数

    ! 可能世界フレームの構造体
    TYPE :: WorldFrame
      INTEGER :: num_worlds ! 世界の数
      LOGICAL :: relations(MAX_WORLDS, MAX_WORLDS) ! アクセス可能性関係 R(w,v)
      LOGICAL :: propositions(MAX_WORLDS, MAX_PROPOSITIONS) ! 各世界での命題の真偽
    END TYPE WorldFrame

    ! モーダル演算子の定義
    TYPE :: ModalOperator
      CHARACTER(LEN=10) :: name ! 演算子名（例："Box", "Diamond", "K_i", "O"）
    END TYPE ModalOperator

  CONTAINS
    ! 公理1：命題論理のトートロジー（Tautology-MOD）
    LOGICAL FUNCTION Tautology(A, B, C)
      LOGICAL, INTENT(IN) :: A, B, C
      ! 例：A → (B → A) のトートロジーをチェック
      Tautology = (.NOT. A .OR. (.NOT. B .OR. A))
      ! 圏論的解釈：ブール代数の恒等射
    END FUNCTION Tautology

    ! 公理2：分配公理（K-MOD）
    LOGICAL FUNCTION K_Axiom(frame, prop_A, prop_B, op_box)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:), prop_B(:) ! 命題A, Bの真偽（各世界）
      TYPE(ModalOperator), INTENT(IN) :: op_box
      LOGICAL :: box_A(MAX_WORLDS), box_B(MAX_WORLDS), box_imp(MAX_WORLDS)
      INTEGER :: i, j
      ! □(A → B) → (□A → □B)
      DO i = 1, frame%num_worlds
        box_A(i) = .TRUE.
        box_B(i) = .TRUE.
        box_imp(i) = .TRUE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            box_A(i) = box_A(i) .AND. prop_A(j)
            box_B(i) = box_B(i) .AND. prop_B(j)
            box_imp(i) = box_imp(i) .AND. (.NOT. prop_A(j) .OR. prop_B(j))
          END IF
        END DO
      END DO
      K_Axiom = ALL(.NOT. box_imp .OR. (.NOT. box_A .OR. box_B))
      ! 圏論的解釈：□は前像関手、代数的解釈：環Rの準同型
    END FUNCTION K_Axiom

    ! 公理3：双対公理（Dual-MOD）
    LOGICAL FUNCTION Dual_Axiom(frame, prop_A, op_box, op_diamond)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:)
      TYPE(ModalOperator), INTENT(IN) :: op_box, op_diamond
      LOGICAL :: box_neg_A(MAX_WORLDS), diamond_A(MAX_WORLDS)
      INTEGER :: i, j
      ! ◇A ↔ ¬□¬A
      DO i = 1, frame%num_worlds
        box_neg_A(i) = .TRUE.
        diamond_A(i) = .FALSE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            box_neg_A(i) = box_neg_A(i) .AND. (.NOT. prop_A(j))
            diamond_A(i) = diamond_A(i) .OR. prop_A(j)
          END IF
        END DO
      END DO
      Dual_Axiom = ALL(diamond_A .EQV. (.NOT. box_neg_A))
      ! 圏論的解釈：◇は□の余関手
    END FUNCTION Dual_Axiom

    ! 公理4：反射公理（T-MOD）
    LOGICAL FUNCTION T_Axiom(frame, prop_A, op_box)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:)
      TYPE(ModalOperator), INTENT(IN) :: op_box
      LOGICAL :: box_A(MAX_WORLDS)
      INTEGER :: i, j
      ! □A → A
      DO i = 1, frame%num_worlds
        box_A(i) = .TRUE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            box_A(i) = box_A(i) .AND. prop_A(j)
          END IF
        END DO
      END DO
      T_Axiom = ALL(.NOT. box_A .OR. prop_A)
      ! 圏論的解釈：反射的圏、群論的解釈：自明な群作用
    END FUNCTION T_Axiom

    ! 公理5：シリアル公理（D-MOD）
    LOGICAL FUNCTION D_Axiom(frame, prop_A, op_box, op_diamond)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:)
      TYPE(ModalOperator), INTENT(IN) :: op_box, op_diamond
      LOGICAL :: box_A(MAX_WORLDS), diamond_A(MAX_WORLDS)
      INTEGER :: i, j
      ! □A → ◇A
      DO i = 1, frame%num_worlds
        box_A(i) = .TRUE.
        diamond_A(i) = .FALSE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            box_A(i) = box_A(i) .AND. prop_A(j)
            diamond_A(i) = diamond_A(i) .OR. prop_A(j)
          END IF
        END DO
      END DO
      D_Axiom = ALL(.NOT. box_A .OR. diamond_A)
      ! 圏論的解釈：シリアル圏、体論的解釈：非零ベクトル
    END FUNCTION D_Axiom

    ! 公理6：推移公理（4-MOD）
    LOGICAL FUNCTION Four_Axiom(frame, prop_A, op_box)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:)
      TYPE(ModalOperator), INTENT(IN) :: op_box
      LOGICAL :: box_A(MAX_WORLDS), box_box_A(MAX_WORLDS)
      INTEGER :: i, j, k
      ! □A → □□A
      DO i = 1, frame%num_worlds
        box_A(i) = .TRUE.
        box_box_A(i) = .TRUE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            box_A(i) = box_A(i) .AND. prop_A(j)
            LOGICAL :: temp_box = .TRUE.
            DO k = 1, frame%num_worlds
              IF (frame%relations(j,k)) THEN
                temp_box = temp_box .AND. prop_A(k)
              END IF
            END DO
            box_box_A(i) = box_box_A(i) .AND. temp_box
          END IF
        END DO
      END DO
      Four_Axiom = ALL(.NOT. box_A .OR. box_box_A)
      ! 圏論的解釈：推移的圏、群論的解釈：推移的軌道
    END FUNCTION Four_Axiom

    ! 公理7：ユークリッド公理（5-MOD）
    LOGICAL FUNCTION Five_Axiom(frame, prop_A, op_box, op_diamond)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:)
      TYPE(ModalOperator), INTENT(IN) :: op_box, op_diamond
      LOGICAL :: diamond_A(MAX_WORLDS), box_diamond_A(MAX_WORLDS)
      INTEGER :: i, j, k
      ! ◇A → □◇A
      DO i = 1, frame%num_worlds
        diamond_A(i) = .FALSE.
        box_diamond_A(i) = .TRUE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            diamond_A(i) = diamond_A(i) .OR. prop_A(j)
            LOGICAL :: temp_diamond = .FALSE.
            DO k = 1, frame%num_worlds
              IF (frame%relations(j,k)) THEN
                temp_diamond = temp_diamond .OR. prop_A(k)
              END IF
            END DO
            box_diamond_A(i) = box_diamond_A(i) .AND. temp_diamond
          END IF
        END DO
      END DO
      Five_Axiom = ALL(.NOT. diamond_A .OR. box_diamond_A)
      ! 圏論的解釈：ユークリッド圏、体論的解釈：ガロア拡大
    END FUNCTION Five_Axiom

    ! 公理8：対称公理（B-MOD）
    LOGICAL FUNCTION B_Axiom(frame, prop_A, op_box, op_diamond)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:)
      TYPE(ModalOperator), INTENT(IN) :: op_box, op_diamond
      LOGICAL :: box_diamond_A(MAX_WORLDS)
      INTEGER :: i, j, k
      ! A → □◇A
      DO i = 1, frame%num_worlds
        box_diamond_A(i) = .TRUE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            LOGICAL :: temp_diamond = .FALSE.
            DO k = 1, frame%num_worlds
              IF (frame%relations(j,k)) THEN
                temp_diamond = temp_diamond .OR. prop_A(k)
              END IF
            END DO
            box_diamond_A(i) = box_diamond_A(i) .AND. temp_diamond
          END IF
        END DO
      END DO
      B_Axiom = ALL(.NOT. prop_A .OR. box_diamond_A)
      ! 圏論的解釈：対称的圏、群論的解釈：アーベル群
    END FUNCTION B_Axiom

    ! 知識論理の公理（例）
    ! 公理9：知識の真公理（KE-MOD）
    LOGICAL FUNCTION KE_Axiom(frame, prop_A, op_k)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:)
      TYPE(ModalOperator), INTENT(IN) :: op_k
      LOGICAL :: k_A(MAX_WORLDS)
      INTEGER :: i, j
      ! K_i A → A
      DO i = 1, frame%num_worlds
        k_A(i) = .TRUE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            k_A(i) = k_A(i) .AND. prop_A(j)
          END IF
        END DO
      END DO
      KE_Axiom = ALL(.NOT. k_A .OR. prop_A)
      ! 圏論的解釈：反射的知識圏
    END FUNCTION KE_Axiom

    ! 公理10：正の内省公理（K4-MOD）
    LOGICAL FUNCTION K4_Axiom(frame, prop_A, op_k)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:)
      TYPE(ModalOperator), INTENT(IN) :: op_k
      LOGICAL :: k_A(MAX_WORLDS), k_k_A(MAX_WORLDS)
      INTEGER :: i, j, k
      ! K_i A → K_i K_i A
      DO i = 1, frame%num_worlds
        k_A(i) = .TRUE.
        k_k_A(i) = .TRUE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            k_A(i) = k_A(i) .AND. prop_A(j)
            LOGICAL :: temp_k = .TRUE.
            DO k = 1, frame%num_worlds
              IF (frame%relations(j,k)) THEN
                temp_k = temp_k .AND. prop_A(k)
              END IF
            END DO
            k_k_A(i) = k_k_A(i) .AND. temp_k
          END IF
        END DO
      END DO
      K4_Axiom = ALL(.NOT. k_A .OR. k_k_A)
      ! 圏論的解釈：推移的知識圏
    END FUNCTION K4_Axiom

    ! 公理11：負の内省公理（K5-MOD）
    LOGICAL FUNCTION K5_Axiom(frame, prop_A, op_k)
      TYPE(WorldFrame), INTENT(IN) :: frame
      LOGICAL, INTENT(IN) :: prop_A(:)
      TYPE(ModalOperator), INTENT(IN) :: op_k
      LOGICAL :: not_k_A(MAX_WORLDS), k_not_k_A(MAX_WORLDS)
      INTEGER :: i, j, k
      ! ¬K_i A → K_i ¬K_i A
      DO i = 1, frame%num_worlds
        not_k_A(i) = .FALSE.
        k_not_k_A(i) = .TRUE.
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            not_k_A(i) = not_k_A(i) .OR. (.NOT. prop_A(j))
            LOGICAL :: temp_not_k = .FALSE.
            DO k = 1, frame%num_worlds
              IF (frame%relations(j,k)) THEN
                temp_not_k = temp_not_k .OR. (.NOT. prop_A(k))
              END IF
            END DO
            k_not_k_A(i) = k_not_k_A(i) .AND. temp_not_k
          END IF
        END DO
      END DO
      K5_Axiom = ALL(.NOT. not_k_A .OR. k_not_k_A)
      ! 圏論的解釈：ユークリッド知識圏、体論的解釈：ガロア固定点
    END FUNCTION K5_Axiom

    ! フレーム条件のチェック（例：反射性）
    LOGICAL FUNCTION IsReflexive(frame)
      TYPE(WorldFrame), INTENT(IN) :: frame
      INTEGER :: i
      IsReflexive = .TRUE.
      DO i = 1, frame%num_worlds
        IF (.NOT. frame%relations(i,i)) THEN
          IsReflexive = .FALSE.
          EXIT
        END IF
      END DO
      ! 圏論的解釈：恒等射の存在
    END FUNCTION IsReflexive

    ! フレーム条件のチェック（例：推移性）
    LOGICAL FUNCTION IsTransitive(frame)
      TYPE(WorldFrame), INTENT(IN) :: frame
      INTEGER :: i, j, k
      IsTransitive = .TRUE.
      DO i = 1, frame%num_worlds
        DO j = 1, frame%num_worlds
          IF (frame%relations(i,j)) THEN
            DO k = 1, frame%num_worlds
              IF (frame%relations(j,k) .AND. .NOT. frame%relations(i,k)) THEN
                IsTransitive = .FALSE.
                RETURN
              END IF
            END DO
          END IF
        END DO
      END DO
      ! 圏論的解釈：射の合成、群論的解釈：推移的軌道
    END FUNCTION IsTransitive

  END MODULE ModalLogicModule

  ! メインルーチン
  USE ModalLogicModule
  TYPE(WorldFrame) :: frame
  TYPE(ModalOperator) :: op_box, op_diamond, op_k
  LOGICAL :: prop_A(MAX_WORLDS), prop_B(MAX_WORLDS)
  INTEGER :: i

  ! 可能世界フレームの初期化（例：3つの世界）
  frame%num_worlds = 3
  frame%relations = .FALSE.
  ! 例：S5フレーム（反射的、推移的、ユークリッド的）
  frame%relations(1,1) = .TRUE.
  frame%relations(1,2) = .TRUE.
  frame%relations(1,3) = .TRUE.
  frame%relations(2,1) = .TRUE.
  frame%relations(2,2) = .TRUE.
  frame%relations(2,3) = .TRUE.
  frame%relations(3,1) = .TRUE.
  frame%relations(3,2) = .TRUE.
  frame%relati
ons(3,3) = .TRUE.

  ! 命題の初期化
  prop_A = [.TRUE., .FALSE., .TRUE.] ! 世界1,3でAが真
  prop_B = [.FALSE., .TRUE., .TRUE.] ! 世界2,3でBが真

  ! モーダル演算子の初期化
  op_box%name = "Box"
  op_diamond%name = "Diamond"
  op_k%name = "K_i"

  ! 公理のテスト
  PRINT *, "Testing Tautology-MOD: ", Tautology(.TRUE., .FALSE., .TRUE.)
  PRINT *, "Testing K-MOD: ", K_Axiom(frame, prop_A, prop_B, op_box)
  PRINT *, "Testing Dual-MOD: ", Dual_Axiom(frame, prop_A, op_box, op_diamond)
  PRINT *, "Testing T-MOD: ", T_Axiom(frame, prop_A, op_box)
  PRINT *, "Testing D-MOD: ", D_Axiom(frame, prop_A, op_box, op_diamond)
  PRINT *, "Testing 4-MOD: ", Four_Axiom(frame, prop_A, op_box)
  PRINT *, "Testing 5-MOD: ", Five_Axiom(frame, prop_A, op_box, op_diamond)
  PRINT *, "Testing B-MOD: ", B_Axiom(frame, prop_A, op_box, op_diamond)
  PRINT *, "Testing KE-MOD: ", KE_Axiom(frame, prop_A, op_k)
  PRINT *, "Testing K4-MOD: ", K4_Axiom(frame, prop_A, op_k)
  PRINT *, "Testing K5-MOD: ", K5_Axiom(frame, prop_A, op_k)
  PRINT *, "Frame Reflexive: ", IsReflexive(frame)
  PRINT *, "Frame Transitive: ", IsTransitive(frame)

END PROGRAM ModalLogic
