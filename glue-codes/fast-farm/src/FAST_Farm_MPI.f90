MODULE FAST_Farm_MPI

   USE NWTC_Library
   USE mpi

   IMPLICIT NONE

   INTEGER(IntKi), PRIVATE :: FF_MPI_Comm  = MPI_COMM_WORLD
   INTEGER(IntKi), PRIVATE :: FF_MPI_Rank  = 0_IntKi
   INTEGER(IntKi), PRIVATE :: FF_MPI_Size  = 1_IntKi
   INTEGER(IntKi), PRIVATE :: FF_MPI_ReKiType = MPI_DATATYPE_NULL
   INTEGER(IntKi), PRIVATE :: FF_MPI_IntKiType = MPI_DATATYPE_NULL
   LOGICAL,        PRIVATE :: FF_MPI_Ready = .FALSE.

CONTAINS

   SUBROUTINE Farm_MPI_Init(ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      LOGICAL                   :: isInit
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Init'

      ErrStat = ErrID_None
      ErrMsg  = ''

      CALL MPI_Initialized(isInit, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Initialized failed.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      IF (.NOT. isInit) THEN
         CALL MPI_Init(ierr)
         IF (ierr /= MPI_SUCCESS) THEN
            CALL SetErrStat(ErrID_Fatal, 'MPI_Init failed.', ErrStat, ErrMsg, RoutineName)
            RETURN
         END IF
      END IF

      CALL MPI_Comm_rank(FF_MPI_Comm, FF_MPI_Rank, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Comm_rank failed.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      CALL MPI_Comm_size(FF_MPI_Comm, FF_MPI_Size, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Comm_size failed.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      CALL Farm_MPI_SetReKiType(ErrStat, ErrMsg)
      IF (ErrStat >= AbortErrLev) RETURN

      CALL Farm_MPI_SetIntKiType(ErrStat, ErrMsg)
      IF (ErrStat >= AbortErrLev) RETURN

      FF_MPI_Ready = .TRUE.
   END SUBROUTINE Farm_MPI_Init

   SUBROUTINE Farm_MPI_Finalize(ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      LOGICAL                   :: isInit, isFinal
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Finalize'

      ErrStat = ErrID_None
      ErrMsg  = ''

      CALL MPI_Initialized(isInit, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Initialized failed.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      IF (.NOT. isInit) RETURN

      CALL MPI_Finalized(isFinal, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Finalized failed.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      IF (.NOT. isFinal) THEN
         CALL MPI_Finalize(ierr)
         IF (ierr /= MPI_SUCCESS) THEN
            CALL SetErrStat(ErrID_Fatal, 'MPI_Finalize failed.', ErrStat, ErrMsg, RoutineName)
            RETURN
         END IF
      END IF

      FF_MPI_Ready = .FALSE.
      FF_MPI_Rank  = 0_IntKi
      FF_MPI_Size  = 1_IntKi
      FF_MPI_ReKiType = MPI_DATATYPE_NULL
      FF_MPI_IntKiType = MPI_DATATYPE_NULL
   END SUBROUTINE Farm_MPI_Finalize

   SUBROUTINE Farm_MPI_Barrier(ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Barrier'

      ErrStat = ErrID_None
      ErrMsg  = ''

      IF (.NOT. FF_MPI_Ready) RETURN

      CALL MPI_Barrier(FF_MPI_Comm, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Barrier failed.', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_Barrier

   SUBROUTINE Farm_MPI_Abort(ExitCode, ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(IN ) :: ExitCode
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Abort'

      ErrStat = ErrID_None
      ErrMsg  = ''

      IF (.NOT. FF_MPI_Ready) RETURN

      CALL MPI_Abort(FF_MPI_Comm, ExitCode, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Abort failed.', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_Abort

   SUBROUTINE Farm_MPI_MaxErrStat(LocalErrStat, GlobalErrStat, ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(IN ) :: LocalErrStat
      INTEGER(IntKi), INTENT(OUT) :: GlobalErrStat
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      INTEGER                 :: ierr
      INTEGER                 :: localDefault, globalDefault
      CHARACTER(*), PARAMETER :: RoutineName = 'Farm_MPI_MaxErrStat'

      ErrStat = ErrID_None
      ErrMsg  = ''

      IF (.NOT. Farm_MPI_UseParallel()) THEN
         GlobalErrStat = LocalErrStat
         RETURN
      END IF

      localDefault = INT(LocalErrStat)
      globalDefault = localDefault

      IF (FF_MPI_IntKiType == MPI_DATATYPE_NULL) THEN
         CALL Farm_MPI_SetIntKiType(ErrStat, ErrMsg)
         IF (ErrStat >= AbortErrLev) THEN
            GlobalErrStat = LocalErrStat
            RETURN
         END IF
      END IF

      CALL MPI_Allreduce(localDefault, globalDefault, 1, FF_MPI_IntKiType, MPI_MAX, FF_MPI_Comm, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         GlobalErrStat = LocalErrStat
         CALL SetErrStat(ErrID_Fatal, 'MPI_Allreduce failed while synchronizing error status.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      GlobalErrStat = INT(globalDefault, IntKi)
   END SUBROUTINE Farm_MPI_MaxErrStat

   SUBROUTINE Farm_MPI_SetReKiType(ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      INTEGER(IntKi)            :: ReKiBytes
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_SetReKiType'

      ErrStat = ErrID_None
      ErrMsg  = ''

      ReKiBytes = STORAGE_SIZE(0.0_ReKi) / 8
      CALL MPI_Type_match_size(MPI_TYPECLASS_REAL, ReKiBytes, FF_MPI_ReKiType, ierr)
      IF (ierr /= MPI_SUCCESS .OR. FF_MPI_ReKiType == MPI_DATATYPE_NULL) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Type_match_size failed for ReKi type.', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_SetReKiType

   SUBROUTINE Farm_MPI_SetIntKiType(ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      INTEGER(IntKi)            :: IntKiBytes
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_SetIntKiType'

      ErrStat = ErrID_None
      ErrMsg  = ''

      IntKiBytes = STORAGE_SIZE(0_IntKi) / 8
      CALL MPI_Type_match_size(MPI_TYPECLASS_INTEGER, IntKiBytes, FF_MPI_IntKiType, ierr)
      IF (ierr /= MPI_SUCCESS .OR. FF_MPI_IntKiType == MPI_DATATYPE_NULL) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Type_match_size failed for IntKi type.', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_SetIntKiType

   SUBROUTINE Farm_MPI_Bcast_ReKi0(val, rootRank, ErrStat, ErrMsg)
      REAL(ReKi),      INTENT(INOUT) :: val
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Bcast_ReKi0'

      ErrStat = ErrID_None
      ErrMsg  = ''
      IF (.NOT. Farm_MPI_UseParallel()) RETURN

      IF (FF_MPI_ReKiType == MPI_DATATYPE_NULL) THEN
         CALL Farm_MPI_SetReKiType(ErrStat, ErrMsg)
         IF (ErrStat >= AbortErrLev) RETURN
      END IF

      IF (rootRank < 0_IntKi .OR. rootRank >= FF_MPI_Size) THEN
         CALL SetErrStat(ErrID_Fatal, 'Invalid MPI root rank in ReKi scalar broadcast.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      CALL MPI_Bcast(val, 1, FF_MPI_ReKiType, rootRank, FF_MPI_Comm, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Bcast failed for ReKi scalar.', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_Bcast_ReKi0

   SUBROUTINE Farm_MPI_Bcast_IntKi0(val, rootRank, ErrStat, ErrMsg)
      INTEGER(IntKi),  INTENT(INOUT) :: val
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Bcast_IntKi0'

      ErrStat = ErrID_None
      ErrMsg  = ''
      IF (.NOT. Farm_MPI_UseParallel()) RETURN

      IF (FF_MPI_IntKiType == MPI_DATATYPE_NULL) THEN
         CALL Farm_MPI_SetIntKiType(ErrStat, ErrMsg)
         IF (ErrStat >= AbortErrLev) RETURN
      END IF

      IF (rootRank < 0_IntKi .OR. rootRank >= FF_MPI_Size) THEN
         CALL SetErrStat(ErrID_Fatal, 'Invalid MPI root rank in IntKi scalar broadcast.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      CALL MPI_Bcast(val, 1, FF_MPI_IntKiType, rootRank, FF_MPI_Comm, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Bcast failed for IntKi scalar.', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_Bcast_IntKi0

   SUBROUTINE Farm_MPI_Bcast_IntKi1(val, rootRank, ErrStat, ErrMsg)
      INTEGER(IntKi),  INTENT(INOUT) :: val(:)
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Bcast_IntKi1'

      ErrStat = ErrID_None
      ErrMsg  = ''
      IF (.NOT. Farm_MPI_UseParallel()) RETURN

      IF (FF_MPI_IntKiType == MPI_DATATYPE_NULL) THEN
         CALL Farm_MPI_SetIntKiType(ErrStat, ErrMsg)
         IF (ErrStat >= AbortErrLev) RETURN
      END IF

      IF (rootRank < 0_IntKi .OR. rootRank >= FF_MPI_Size) THEN
         CALL SetErrStat(ErrID_Fatal, 'Invalid MPI root rank in IntKi array broadcast.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      CALL MPI_Bcast(val, SIZE(val), FF_MPI_IntKiType, rootRank, FF_MPI_Comm, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Bcast failed for IntKi array(1D).', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_Bcast_IntKi1

   SUBROUTINE Farm_MPI_Bcast_ReKi1(val, rootRank, ErrStat, ErrMsg)
      REAL(ReKi),      INTENT(INOUT) :: val(:)
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Bcast_ReKi1'

      ErrStat = ErrID_None
      ErrMsg  = ''
      IF (.NOT. Farm_MPI_UseParallel()) RETURN

      IF (FF_MPI_ReKiType == MPI_DATATYPE_NULL) THEN
         CALL Farm_MPI_SetReKiType(ErrStat, ErrMsg)
         IF (ErrStat >= AbortErrLev) RETURN
      END IF

      IF (rootRank < 0_IntKi .OR. rootRank >= FF_MPI_Size) THEN
         CALL SetErrStat(ErrID_Fatal, 'Invalid MPI root rank in ReKi array(1D) broadcast.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      CALL MPI_Bcast(val, SIZE(val), FF_MPI_ReKiType, rootRank, FF_MPI_Comm, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Bcast failed for ReKi array(1D).', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_Bcast_ReKi1

   SUBROUTINE Farm_MPI_Bcast_ReKi2(val, rootRank, ErrStat, ErrMsg)
      REAL(ReKi),      INTENT(INOUT) :: val(:,:)
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Bcast_ReKi2'

      ErrStat = ErrID_None
      ErrMsg  = ''
      IF (.NOT. Farm_MPI_UseParallel()) RETURN

      IF (FF_MPI_ReKiType == MPI_DATATYPE_NULL) THEN
         CALL Farm_MPI_SetReKiType(ErrStat, ErrMsg)
         IF (ErrStat >= AbortErrLev) RETURN
      END IF

      IF (rootRank < 0_IntKi .OR. rootRank >= FF_MPI_Size) THEN
         CALL SetErrStat(ErrID_Fatal, 'Invalid MPI root rank in ReKi array(2D) broadcast.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      CALL MPI_Bcast(val, SIZE(val), FF_MPI_ReKiType, rootRank, FF_MPI_Comm, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Bcast failed for ReKi array(2D).', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_Bcast_ReKi2

   SUBROUTINE Farm_MPI_Bcast_ReKi3(val, rootRank, ErrStat, ErrMsg)
      REAL(ReKi),      INTENT(INOUT) :: val(:,:,:)
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      INTEGER(IntKi)            :: ierr
      CHARACTER(*), PARAMETER   :: RoutineName = 'Farm_MPI_Bcast_ReKi3'

      ErrStat = ErrID_None
      ErrMsg  = ''
      IF (.NOT. Farm_MPI_UseParallel()) RETURN

      IF (FF_MPI_ReKiType == MPI_DATATYPE_NULL) THEN
         CALL Farm_MPI_SetReKiType(ErrStat, ErrMsg)
         IF (ErrStat >= AbortErrLev) RETURN
      END IF

      IF (rootRank < 0_IntKi .OR. rootRank >= FF_MPI_Size) THEN
         CALL SetErrStat(ErrID_Fatal, 'Invalid MPI root rank in ReKi array(3D) broadcast.', ErrStat, ErrMsg, RoutineName)
         RETURN
      END IF

      CALL MPI_Bcast(val, SIZE(val), FF_MPI_ReKiType, rootRank, FF_MPI_Comm, ierr)
      IF (ierr /= MPI_SUCCESS) THEN
         CALL SetErrStat(ErrID_Fatal, 'MPI_Bcast failed for ReKi array(3D).', ErrStat, ErrMsg, RoutineName)
      END IF
   END SUBROUTINE Farm_MPI_Bcast_ReKi3

   PURE FUNCTION Farm_MPI_Rank() RESULT(rankId)
      INTEGER(IntKi) :: rankId
      rankId = FF_MPI_Rank
   END FUNCTION Farm_MPI_Rank

   PURE FUNCTION Farm_MPI_Size() RESULT(nRanks)
      INTEGER(IntKi) :: nRanks
      nRanks = FF_MPI_Size
   END FUNCTION Farm_MPI_Size

   PURE FUNCTION Farm_MPI_UseParallel() RESULT(parallelFlag)
      LOGICAL :: parallelFlag
      parallelFlag = FF_MPI_Ready .AND. (FF_MPI_Size > 1)
   END FUNCTION Farm_MPI_UseParallel

   PURE FUNCTION Farm_MPI_OwnerRank(turbineIndex) RESULT(ownerRank)
      INTEGER(IntKi), INTENT(IN) :: turbineIndex
      INTEGER(IntKi)             :: ownerRank

      ownerRank = MOD(MAX(0_IntKi, turbineIndex - 1_IntKi), MAX(1_IntKi, FF_MPI_Size))
   END FUNCTION Farm_MPI_OwnerRank

   PURE FUNCTION Farm_MPI_OwnsTurbine(turbineIndex) RESULT(isOwner)
      INTEGER(IntKi), INTENT(IN) :: turbineIndex
      LOGICAL                    :: isOwner

      isOwner = (Farm_MPI_OwnerRank(turbineIndex) == FF_MPI_Rank)
   END FUNCTION Farm_MPI_OwnsTurbine

END MODULE FAST_Farm_MPI
