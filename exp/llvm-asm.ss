#lang typed-scheme
(require (planet dherman/types:2))

(define-type-alias XXX Boolean)

(define-datatype ArithmeticOp
  [Add #:constant ADD]
  [Sub #:constant SUB]
  [Mul #:constant MUL]
  [UDiv #:constant UDIV]
  [SDiv #:constant SDIV]
  [FDiv #:constant FDIV]
  [URem #:constant UREM]
  [SRem #:constant SREM]
  [FRem #:constant FREM])

(define-datatype LogicalOp
  [Shl #:constant SHL]
  [LShr #:constant LSHR]
  [AShr #:constant ASHR]
  [And #:constant AND]
  [Or #:constant OR]
  [XOr #:constant XOR])

(define-datatype CastOp
  [Trunc #:constant TRUNC]
  [ZExt #:constant ZEXT]
  [SExt #:constant SEXT]
  [FPTrunc #:constant FPTRUNC]
  [FPExt #:constant FPEXT]
  [Bitcast #:constant BITCAST]
  [UItoFP #:constant UITOFP]
  [SItoFP #:constant SITOFP]
  [FPtoUI #:constant FPTOUI]
  [FPtoSI #:constant FPTOSI]
  [INTtoPTR #:constant INTTOPTR]
  [PTRtoINT #:constant PTRTOPTR])

(define-datatype IPredicate
  [Eq #:constant EQ]   [Ne #:constant NE]
  [SLt #:constant SLT] [SGt #:constant SGT]
  [SLe #:constant SLE] [SGe #:constant SGE]
  [ULt #:constant ULT] [UGt #:constant UGT]
  [ULe #:constant ULE] [UGe #:constant UGE])

(define-datatype FPredicate
  [OEq #:constant OEQ] [ONe #:constant ONE]
  [OLt #:constant OLT] [OGt #:constant OGT]
  [OLe #:constant OLE] [OGe #:constant OGE]
  [ORd #:constant ORD] [UNo #:constant UNO]
  [FUEq #:constant FUEQ] [FUNe #:constant FUNE]
  [FULt #:constant FULT] [FUGt #:constant FUGT]
  [FULe #:constant FULE] [FUGe #:constant FUGE]
  [True #:constant TRUE]
  [False #:constant FALSE])

(define-type-alias LocalName
  (U LocalVar StringConstant PCTStringConstant))

(define-type-alias OptLocalName
  (Option LocalName))

(define-type-alias OptAddrSpace
  (Option XXX)) ; ADDRSPACE '(' EUINT64VAL ')'

(define-type-alias OptLocalAssign
  (Option LocalAssign))

; LocalName '=' ...
(define-struct: LocalAssign
  ([name : LocalName]))

; LocalVal_Id '=' ...
(define-struct: LocalNumber
  ([id : LocalVarId]))

(define-type-alias GlobalName
  (U GlobalVar ATStringConstant))

(define-type-alias OptGlobalAssign
  (Option GlobalAssign))

; GlobalName '=' ...
(define-struct: GlobalAssign
  ([name : GlobalName]))

(define-datatype GVInternalLinkage
  [Internal #:constant internal]
  [Weak #:constant weak]
  [LinkOnce #:constant linkonce]
  [Appending #:constant appending]
  [DLLExport #:constant dllexport]
  [Common #:constant common])

(define-datatype GVExternalLinkage
  [DLLImport #:constant dllimport]
  [Extern_Weak #:constant extern_weak]
  [External #:constant external])

(define-datatype GVVisibilityStyle
  [Default #:constant default]
  [Hidden #:constant hidden]
  [Protected #:constant protected])

(define-datatype FunctionDeclareLinkage
  [FDL_DLLImport #:constant fdl:dllimport]
  [FDL_Extern_Weak #:constant fdl:extern_weak])

(define-datatype FunctionDefineLinkage
  [FDL_Internal #:constant fdl:internal]
  [FDL_LinkOnce #:constant fdl:linkonce]
  [FDL_Weak #:constant fdl:weak]
  [FDL:DLLExport #:constant fdl:dllexport])

(define-datatype AliasLinkage
  [AL_Weak #:constant al:weak]
  [AL_Internal #:constant al:internal])

(define-datatype CallingConv
  [CCC #:constant ccc]
  [FastCC #:constant fastcc]
  [ColdCC #:constant coldcc]
  [X86Std #:constant x86stdcc]
  [X86Fast #:constant x86fastcc]
  [cc ([n : EUINT64VAL])])

(define-datatype Attribute
  [ZeroExt #:constant zeroext]
  [SignExt #:constant signext]
  [InReg #:constant inreg]
  [SRet #:constant sret]
  [ByVal #:constant byval]
  [NoAlias #:constant noalias]
  [Nest #:constant nest]
  [Align ([n : EUINT64VAL])])

; space separated
(define-type-alias OptAttributes
  (Listof Attribute))

(define-datatype RetAttr
  [RInReg #:constant r:inreg]
  [RZeroExt #:constant r:zeroext]
  [RSignExt #:constant r:signext]
  [RNoAlias #:constant r:noalias])

; space separated
(define-type-alias OptRetAttrs
  (Listof RetAttr))

(define-datatype FuncAttr
  [NoReturn #:constant noreturn]
  [NoUnwind #:constant nounwind]
  [FA_InReg #:constant fa:inreg]
  [FA_ZeroExt #:constant fa:zeroext]
  [FA_SignExt #:constant fa:signext]
  [ReadNone #:constant readnone]
  [ReadOnly #:constant readonly]
  [NoInline #:constant noinline]
  [AlwaysInline #:constant alwaysinline]
  [OptSize #:constant optsize]
  [SSP #:constant ssp]
  [SSPReq #:constant sspreq])

; space sep
(define-type-alias OptFuncAttrs
  (Listof FuncAttr))

; GC ...
(define-struct: GC
  ([name : StringConstant]))

(define-struct: _Align
  ([n : EUINT64VAL]))

(define-type-alias OptAlign
  (Option _Align))

; print with comma before
(define-type-alias OptCAlign
  OptAlign)

; SECTION ...
(define-struct: SectionString
  ([sc : StringConstant])) ; constraints on what strings allowed

(define-type-alias OptSection
  (Option SectionString))

(define-type-alias GlobalVarAttributes
  (Listof GlobalVarAttribute))

(define-type-alias GlobalVarAttribute
  (U _Align SectionString))

(define-datatype PrimType
  [IntType #:constant inttype]
  [Float #:constant float]
  [Double #:constant double]
  [PPC_FP128 #:constant ppc_fp128]
  [FP128 #:constant fp128]
  [X86_FP128 #:constant x86_fp128]
  [Label #:constant label])

(define-datatype Types
  [Opaque #:constant opaque]
  [Prim ([pt : PrimType])]
  ; <st> <space> *
  [Ptr ([st : Types]
        [space : OptAddrSpace])]
  [SymVal ([ref : SymoblicValueRef])]
  ; \\ <ref>
  [UpRef ([ref : EUINT64VAL])]
  ; <ret> ( <list> ) <funcattrs>
  [Fun ([ret : ResultTypes]
        [args : ArgTypeListI]
        [attrs : OptFuncAttrs])]
  ; [ <#> x <et> ]
  [Array ([n : EUINT64VAL]
          [et : Types])]
  ; < <#> x <et> ]
  [Vector ([n : EUINT64VAL]
           [et : Types])]
  ; { <ts> }
  [Struct ([ts : OptTypeListI])])

(define-constant [void : Void])

(define-type-alias ResultTypes
  (U Types Void))

; Next: ArgTypeList