MODULE FAST_Farm_MPI

   USE NWTC_Library

   IMPLICIT NONE

CONTAINS

   SUBROUTINE Farm_MPI_Init(ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Init

   SUBROUTINE Farm_MPI_Finalize(ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Finalize

   SUBROUTINE Farm_MPI_Barrier(ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Barrier

   SUBROUTINE Farm_MPI_Abort(ExitCode, ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(IN ) :: ExitCode
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Abort

   SUBROUTINE Farm_MPI_MaxErrStat(LocalErrStat, GlobalErrStat, ErrStat, ErrMsg)
      INTEGER(IntKi), INTENT(IN ) :: LocalErrStat
      INTEGER(IntKi), INTENT(OUT) :: GlobalErrStat
      INTEGER(IntKi), INTENT(OUT) :: ErrStat
      CHARACTER(*),   INTENT(OUT) :: ErrMsg

      GlobalErrStat = LocalErrStat
      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_MaxErrStat

   SUBROUTINE Farm_MPI_Bcast_ReKi0(val, rootRank, ErrStat, ErrMsg)
      REAL(ReKi),      INTENT(INOUT) :: val
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Bcast_ReKi0

   SUBROUTINE Farm_MPI_Bcast_IntKi0(val, rootRank, ErrStat, ErrMsg)
      INTEGER(IntKi),  INTENT(INOUT) :: val
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Bcast_IntKi0

   SUBROUTINE Farm_MPI_Bcast_IntKi1(val, rootRank, ErrStat, ErrMsg)
      INTEGER(IntKi),  INTENT(INOUT) :: val(:)
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Bcast_IntKi1

   SUBROUTINE Farm_MPI_Bcast_ReKi1(val, rootRank, ErrStat, ErrMsg)
      REAL(ReKi),      INTENT(INOUT) :: val(:)
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Bcast_ReKi1

   SUBROUTINE Farm_MPI_Bcast_ReKi2(val, rootRank, ErrStat, ErrMsg)
      REAL(ReKi),      INTENT(INOUT) :: val(:,:)
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Bcast_ReKi2

   SUBROUTINE Farm_MPI_Bcast_ReKi3(val, rootRank, ErrStat, ErrMsg)
      REAL(ReKi),      INTENT(INOUT) :: val(:,:,:)
      INTEGER(IntKi),  INTENT(IN   ) :: rootRank
      INTEGER(IntKi),  INTENT(  OUT) :: ErrStat
      CHARACTER(*),    INTENT(  OUT) :: ErrMsg

      ErrStat = ErrID_None
      ErrMsg  = ''
   END SUBROUTINE Farm_MPI_Bcast_ReKi3

   PURE FUNCTION Farm_MPI_Rank() RESULT(rankId)
      INTEGER(IntKi) :: rankId
      rankId = 0_IntKi
   END FUNCTION Farm_MPI_Rank

   PURE FUNCTION Farm_MPI_Size() RESULT(nRanks)
      INTEGER(IntKi) :: nRanks
      nRanks = 1_IntKi
   END FUNCTION Farm_MPI_Size

   PURE FUNCTION Farm_MPI_UseParallel() RESULT(parallelFlag)
      LOGICAL :: parallelFlag
      parallelFlag = .FALSE.
   END FUNCTION Farm_MPI_UseParallel

   PURE FUNCTION Farm_MPI_OwnerRank(turbineIndex) RESULT(ownerRank)
      INTEGER(IntKi), INTENT(IN) :: turbineIndex
      INTEGER(IntKi)             :: ownerRank

      ownerRank = 0_IntKi
   END FUNCTION Farm_MPI_OwnerRank

   PURE FUNCTION Farm_MPI_OwnsTurbine(turbineIndex) RESULT(isOwner)
      INTEGER(IntKi), INTENT(IN) :: turbineIndex
      LOGICAL                    :: isOwner

      isOwner = .TRUE.
   END FUNCTION Farm_MPI_OwnsTurbine

END MODULE FAST_Farm_MPI
