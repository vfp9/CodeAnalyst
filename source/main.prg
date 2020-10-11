*!******************************************************************************
*!* ����.......: MAIN.PRG
*!* ����.......: Andrew MacNeill
*!* ����.......: 
*!* ��Ȩ.......: aksel
*!* ����汾...: Visual FoxPro09.00.0000.7423
*!* ˵��.......: 
*!* �﷨.......:
*!* ����.......: tcFile		�ַ��ͣ���ѡ�������ļ��������ʡ�Դ˲�������ʾ��ǰ����
*!*              tlSilent	�߼��ԣ���ѡ�������Ƿ���ʾ��������
*!* ����ֵ.....:
*!* ������ʷ...:
*!*              2020.10.04	xinjie ��ʼ�޸�
*!******************************************************************************

Lparameters tcFile, tlSilent

If Pcount() = 1
	m.tlSilent = .F.
Endif

If Pcount() = 0
	m.tcFile = ""
Endif

External Array taArray

Local llRunning

m.llRunning = Pemstatus(_Screen, "_Analyst", 5)

If Not m.llRunning
	*- ȷ�����Ƿ�ʵ������һ���������е�Ӧ�ó��� ��Ҫ����Ӧ�ó���λ��HOME������
	If Upper(Justext(Sys(16))) = "APP" Then
		_Screen.Newobject("_Analyst", "_codeAnalyzer", "MAIN.PRG", Sys(16))

	Else
		_Screen.Newobject("_analyst", "_codeAnalyzer", "MAIN.PRG")
	Endif

	m.llRunning = .T.
EndIf

With _Screen._Analyst
	.IsDirectory	= .F.
	.cDirectory		= ""
EndWith 

*!* ���������������ļ��С���������
If Upper(m.tcFile) = "DIRECTORY"
	If Directory(m.tlSilent)
		With _Screen._Analyst
			.IsDirectory	= .T.
			.cDirectory		= m.tlSilent
			
			.BuildFakeProject(m.tlSilent)
		EndWith 
		
		m.tcFile = _Screen._Analyst.cFile
		m.tlSilent = .T.

	Else
		Messagebox("Ŀ¼ " + m.tlSilent + " �����ڡ�", 0 + 16, "�������")
		m.tlSilent	= .F.
		m.tcFile	= ""
	Endif
Endif

If m.tlSilent
	_Screen._Analyst.lProjectRun = .T.
Endif
*-����Ƿ񴫵��˲����� ���û��ͨ���������������װ�ڲ˵���
*-����Ѿ���װ�˷���������������ζ�Ҫ����������

If Pcount() > 0 And Upper(m.tcFile)="-CONFIG"
	_Screen._Analyst.Configure()

Else
	If Pcount() = 1 Or m.llRunning Then
		_Screen._Analyst.Analyze(m.tcFile)
	Endif
Endif

Define Class _codeAnalyzer As Custom
	cFile			= ""
	cSetESC			= ""						&& On("ESCAPE")
	csetesclabel	= ""						&& On("KEY", "ESC")
	cMainProgram	= ""
	cHomeDir		= ""						&& �洢 APP ���ڵ�·��, ��� ����UI �� ���� ��ťʱ���洢�� VFP ��Դ�ļ��С�
	isdirectory		= .F.						&& �Ƿ����ָ����Ŀ¼
	cDirectory		= ""						&& ��������Ŀ¼��
	lLineRules		= .F.						&& �Ƿ���������ÿһ�У�������ڴ����з�����������򽫸��Ĵ�����Ϊ .T.��.F.��������ٶȣ�
	cLine			= ""						&& �������Ĵ����У����ⲿ����
	nLine			= 0							&& �������Ĵ����е��кţ����ⲿ����
	cFuncName		= ""						&& �������Ĵ�������λ�ڵĺ������������¼��������ⲿ����
	oTherm			= .NULL.					&& ��������UI�Ķ�������
	oObject			= .NULL.					&& ����������Ķ�������
	cObject			= ""						&& ����������Ķ�����(Object.Name)
	cCode			= ""						&& �������Ĵ���Ƭ�Σ����ⲿ����
	cFontString		= "Tahoma,8,N"				&& �洢 UI �е���������, ��� ����UI �� ���� ��ťʱ���洢�� VFP ��Դ�ļ��С�
	cError			= ""						&& �洢���󣬿��ⲿ����
	cRuleDir		= ""						&& �洢����������·��, ��� ����UI �� ���� ��ťʱ���洢�� VFP ��Դ�ļ��С�
	cResetFile		= ""						&& �洢��������� XML ���ļ�, ��� ����UI �� ���� ��ťʱ���洢�� VFP ��Դ�ļ��С�
	cAnalysisCursor	= ""						&& �洢��������� Cursor ������
	nFuncLines		= 0							&& �������Ĵ����������
	cWarningID		= ""
	nFileLines		= 0							&& �������Ĵ������������
	cMessage		= ""
	lDisplayMessage	= .T.
	lDisplayForm	= .T.						&& ȷ������������Ƿ���ʾ�����ⲿ����
	lUseDefaultDir	= .T.
	lProjectRun		= .F.
	cTable			= ""						&& δʹ��
	cClassName		= Space(0)						&& ���������������


	Procedure Reset
		*!******************************************************************************
		*!* ����.......: Reset
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: Reset
		*!* �﷨.......:
		*!* ����.......: ��
		*!* ����ֵ.....: ��
		*!******************************************************************************
		If Not Empty(This.cAnalysisCursor)
			If Used(This.cAnalysisCursor)
				Local lc
				m.lc = Set("SAFETY")
				
				Set Safety Off
				Zap In (This.cAnalysisCursor)
				Set Safety &lc
			Endif
		Endif
	Endproc

	Procedure SetPrefs
		*!******************************************************************************
		*!* ����.......: SetPrefs
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ���ó��������ƫ��
		*!* �﷨.......:
		*!* ����.......: ��
		*!* ����ֵ.....: �߼��͡�.T.���ɹ���.F.��ʧ��
		*!******************************************************************************
		Local nSelect
		Local lcRes
		m.lcRes = "ANALYST"

		Local lSuccess
		Local nMemoWidth
		Local nCnt
		Local cData

		_AnalystResetXML		= This.cResetFile
		_AnalystFontString		= This.cFontString
		_Analysthomedir			= This.cHomeDir
		_AnalystRuledir			= This.cRuleDir

		If This.lUseDefaultDir
			_AnalystRuledir = ""
		Endif

		m.nSelect = Select()

		m.lSuccess = .F.

		* ȷ����Դ�ļ����ڲ��Ҳ���ֻ����
		m.nCnt = Adir(aFileList, Sys(2005))

		If m.nCnt > 0 And Atc('R', aFileList[1, 5]) == 0
			Use (Sys(2005)) In Select("FOXRESOURCE") Shared Again Alias FoxResource

			If Used("FoxResource") And !Isreadonly("FoxResource")
				m.nMemoWidth = Set('MEMOWIDTH')
				Set Memowidth To 255

				Select FoxResource
				Locate For Upper(Alltrim(Type)) == "PREFW" And Upper(Alltrim(Id)) == m.lcRes And Empty(Name)

				If !Found()
					Append Blank In FoxResource
					Replace In FoxResource;
						TYPE		With "PREFW", ;
						ID			With m.lcRes, ;
						READONLY	With .F. ;
						
				Endif

				If !FoxResource.ReadOnly
					Save To Memo Data All Like _Analyst*

					Replace IN FoxResource;
						UPDATED		With Date(), ;
						ckval		With Val(Sys(2007, FoxResource.Data)) ;
						
					m.lSuccess = .T.
				Endif

				Set Memowidth To (nMemoWidth)
				Use In FoxResource
			Endif
		Endif

		Select (m.nSelect)

		Return m.lSuccess
	Endproc

	Procedure GetPrefs
		*!******************************************************************************
		*!* ����.......: GetPrefs
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ��ȡ��������ƫ��
		*!* �﷨.......:
		*!* ����.......: ��
		*!* ����ֵ.....: �߼�ֵ��.T.���ɹ���.F.��ʧ��
		*!******************************************************************************
		Local nSelect
		Local lcRes
		m.lcRes = "ANALYST"

		Local lSuccess
		Local nMemoWidth

		m.nSelect	= Select()
		m.lSuccess	= .F.

		If Empty(This.cRuleDir)
			This.cRuleDir = Home()
		Endif

		If File(Sys(2005))    && resource file not found.
			Use (Sys(2005)) In Select("FOXRESOURCE") Shared Again Alias FoxResource

			If Used("FoxResource")
				m.nMemoWidth = Set('MEMOWIDTH')
				Set Memowidth To 255

				Select FoxResource
				Locate For Upper(Alltrim(Type)) == "PREFW" AND Upper(Alltrim(Id)) == m.lcRes AND !Deleted()

				If Found() And !Empty(Data) And ckval == Val(Sys(2007, Data)) And Empty(Name)
					Restore From Memo Data Additive

					This.cFontString = m._AnalystFontString

					If Type("_ANALYSTRuleDIR") = "C"
						If Not Empty(m._AnalystRuledir)
							This.lUseDefaultDir	= .F.
							This.cRuleDir		= m._AnalystRuledir

						Else
							This.lUseDefaultDir	= .T.
							This.cRuleDir		= Curdir()
						Endif

					Else
						This.lUseDefaultDir	= .T.
						This.cRuleDir		= Curdir()
					Endif

					If Type("_ANALYSTHomeDIR") = "C"
						If Not Empty(m._Analysthomedir)
						Else
							This.cHomeDir = Curdir()
						Endif

					Else
						This.cHomeDir = Curdir()
					Endif

					If Type("_AnalystResetXML") = "C"
						If Not Empty(m._AnalystResetXML)
							This.cResetFile = m._AnalystResetXML
						Else
							This.cResetFile = ""
						Endif
					Else
						This.cResetFile = ""
					Endif


					m.lSuccess = .T.
				Endif

				Set Memowidth To (nMemoWidth)

				Use In FoxResource
			Endif
		Endif

		Select (m.nSelect)

		Return m.lSuccess
	Endproc

	Procedure CreateRuleTable
		*!******************************************************************************
		*!* ����.......: CreateRuleTable
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ����������������
		*!* �﷨.......:
		*!* ����.......: ��
		*!* ����ֵ.....: ��
		*!******************************************************************************
		If Not File(This.cRuleDir + "CODERULE.DBF")
			Local lnArea
			m.lnArea = Select()

			Select 0

			Create Table (This.cRuleDir + "CODERULE.DBF") (;
				TYPE C(1),;
				NAME C(30),;
				ACTIVE L,;
				DESCRIPT M,;
				Script M,;
				PROGRAM M,;
				CLASSLIB C(30),;
				Classname C(50),;
				TIMESTAMP T,;
				UniqueID C(10);
				)

			If Not Used("_CODERULE")
				Use _CODERULE In 0
			Endif

			Select _CODERULE

			Scan
				Scatter Memvar Memo
				Insert Into (This.cRuleDir + "CODERULE") From Memvar
			Endscan

			Use In Select("_CODERULE")
		Endif
	Endproc

	Procedure Destroy
		If Not Isnull(This.oTherm)
			This.oTherm.Hide()
			This.oTherm.Release()
		Endif

		If Not Empty(Prmbar("_MTOOLS", 5942)) Then
			Release Bar 5942 Of _MTOOLS
		Endif
	Endproc

	Procedure Configure
		*!******************************************************************************
		*!* ����.......: Configure
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ��ʾ������������ñ�
		*!* �﷨.......:
		*!* ����.......: ��
		*!* ����ֵ.....: ��
		*!******************************************************************************
		Local lnArea
		m.lnArea = Select()

		If Not Used("CODERULE")
			*- ������ʹ��cHomeDir����...�ٴβ�Ҫ����HOME����
			Use (This.cRuleDir + "CODERULE") In 0
		Endif

		Do Form ConfigureAnalyst
		Select (m.lnArea)
	Endproc

	Procedure AddWarning
		*!******************************************************************************
		*!* ����.......: AddWarning
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ��ϵͳ��Ӿ�����Ϣ������ʾ��չʾ�����������ı���
		*!* �﷨.......:
		*!* ����.......: tcWarning		�ַ��͡�������ʾ�ľ�����Ϣ
		*!*              tcType			�ַ��͡�
		*!* ����ֵ.....: ��
		*!******************************************************************************
		Lparameters tcWarning, tcType
		If Not Empty(This.aWarnings(1, 1))
			Dimension This.aWarnings(Alen(This.aWarnings, 1) + 1, 3)
		Endif

		Dimension This.aWarnings(Alen(This.aWarnings, 1), 3)

		This.aWarnings(Alen(This.aWarnings, 1), 1) = m.tcWarning

		If Empty(This.cFile)
			This.cFile = "δ֪"
		Endif

		This.aWarnings(Alen(This.aWarnings, 1), 2) = This.cFile
		This.aWarnings(Alen(This.aWarnings, 1), 3) = This.cWarningID

		If Used(This.cAnalysisCursor)
			Local lnArea
			m.lnArea = Select()

			Select (This.cAnalysisCursor)
			This.AddWarningCursor(m.tcWarning)
			*!* REPLACE warnings WITH warnings + tcWarning +CHR(13)+CHR(10)
			Select (m.lnArea)
		Endif
	Endproc

	Procedure AddWarningCursor
		Lparameters tcWarning
		Local lcFile,lcID,lcFunc,lcType
		m.lcType	= "����"
		m.lcID		= This.cWarningID
		m.lcFile	= This.cFile
		m.lcFunc	= This.cFuncName
		m.lnLine	= This.nLine

		If Not Empty(This.cObject)
			m.lcFunc = Trim(This.cObject) + "." + m.lcFunc
		Endif

		If Empty(This.cAnalysisCursor)
			This.BuildAnalysisCursor()
		Endif

		m.lcFileType = This.GetFileType()

		Insert Into (This.cAnalysisCursor) (cfunc, cprog, cType, cFileType, cWarning, nLine, warnings) ;
			VALUES (m.lcFunc, m.lcFile, m.lcType, m.lcFileType, m.lcID, m.lnLine, m.tcWarning)
	Endproc

	Procedure GetFileType
		*!******************************************************************************
		*!* ����.......: GetFileType
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ��ȡ�������ļ����ļ�����
		*!* �﷨.......:
		*!* ����.......:
		*!* ����ֵ.....:
		*!* ��ע.......: �������������ļ���չ���ж��ļ����ͣ�����
		*!******************************************************************************
		Local lcExt,lcRet
		m.lcRet = "����"
		m.lcExt = Upper(Justext(This.cFile))

		Do Case
			Case m.lcExt = "VCX"
				m.lcRet = "��"

			Case m.lcExt = "SCX"
				m.lcRet = "��"

			Case m.lcExt = "MNX"
				m.lcRet = "�˵�"

			Case m.lcExt = "PRG"
				*!* ������Ҫ�Ľ��������ļ������ж��� ���򡢺�������������
				m.lcRet = "����"

			Case m.lcExt = "APP"
				m.lcRet = "Apps"
		Endcase

		Return m.lcRet
	Endproc

	Procedure AddMessage
		Lparameters tcMsg
		This.cMessage = This.cMessage + Iif(Empty(This.cMessage), "", Chr(13) + Chr(10)) + m.tcMsg
	Endproc

	Procedure Init
		*-�ٴβ�Ҫ����HOME��������Ӧʹ��SYS��16���� ���ڽ���/�������ư���
		*-��ȡ���ݵ�homedir
		Local lcProgram
		Local lcSetExact
		m.lcSetExact=Set("EXACT")

		Set Exact Off

		If Sys(16) = "PROCEDURE"
			m.lcProgram = Alltrim(Substr(Sys(16), Atc(" ", Sys(16), 2) + 1))

		Else
			m.lcProgram = Alltrim(Strextract(Sys(16), " ", " ", 2, 2))
		Endif

		This.cHomeDir = Justpath(lcProgram) + "\"

		If Not Pemstatus(This, "aCode", 5)
			This.AddProperty("aCode(1, 4)")
		Endif

		If Not Pemstatus(This, "awarnings", 5)
			This.AddProperty("awarnings(1, 3)")
		Endif

		This.aWarnings(1, 1)	= ""
		This.aWarnings(1, 2)	= ""
		This.aWarnings(1, 3)	= ""
		This.aCode(1, 1)		= ""

		If Not Pemstatus(This, "aRules", 5)
			This.AddProperty("aRules(1, 5)")
		Endif

		This.GetPrefs()
		This.CreateRuleTable()
		This.LoadRules()

		Local lcDir
		m.lcDir = "DO ('" + This.cHomeDir + "ANALYST.APP')"

		Define Bar 5942 Of _MTOOLS Prompt "�������..." After  _MTL_TOOLBOX
		On Selection Bar 5942 Of _MTOOLS &lcDir

		Set Exact &lcSetExact
	Endproc

	Procedure BuildAnalysisCursor
		*!******************************************************************************
		*!* ����.......: BuildAnalysisCursor
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ����������
		*!* �﷨.......:
		*!* ����.......:
		*!* ����ֵ.....:
		*!******************************************************************************
		Local lnArea
		m.lnArea = Select()

		Select 0
		This.cAnalysisCursor = Sys(2015)

		Create Cursor (This.cAnalysisCursor) (;
			cFileType C(10),;
			cfunc C(50),;
			cprog C(120),;
			cClass C(50),;
			cType C(10),;
			nLine N(6),;
			cWarning C(10),;
			warnings M;
			)

		Local lc
		m.lc = Set("COLLATE", "TO")
		Set Collate To "MACHINE"

		Index On cfunc+cprog+cClass Tag funcProg

		Set Collate To (m.lc)

		Select (m.lnArea)
	Endproc

	Procedure AddToCursor
		*!******************************************************************************
		*!* ����.......: AddToCursor
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: �������������Ϣ���������
		*!* �﷨.......:
		*!* ����.......:
		*!* ����ֵ.....:
		*!******************************************************************************
		Lparameters tcFunc, tcProg, tcClass,tcType
		*% Add tcClass
		If Empty(m.tcType)
			m.tcType = Justext(m.tcProg)
		Endif

		If Empty(m.tcType)
			m.tcType = "δ֪"
		Endif

		If Empty(This.cAnalysisCursor)
			This.BuildAnalysisCursor()

		Else
			If Not Used(This.cAnalysisCursor)
				This.BuildAnalysisCursor()
			Endif
		Endif

		If Empty(m.tcFunc)
			m.tcFunc = This.cFile
		Endif

		If Empty(m.tcProg)
			m.tcProg = This.cFuncName
		Endif

		If Empty(m.tcClass)
			m.tcClass = This.cClassName
		Endif

		m.tcFunc = Padr(m.tcFunc, 50)
		m.tcProg = Padr(m.tcProg, 125)

		If Not Seek(m.tcFunc+tcProg+tcClass, This.cAnalysisCursor)
			Insert Into (This.cAnalysisCursor) (cfunc, cprog, cClass, cType) ;
										VALUES (m.tcFunc, m.tcProg, m.tcClass, m.tcType)
		Endif
	Endproc

	Procedure LoadRules
		*!******************************************************************************
		*!* ����.......: LoadRules
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: �����������
		*!* �﷨.......:
		*!* ����.......: ��
		*!* ����ֵ.....: ��
		*!******************************************************************************
		If File(This.cRuleDir + "CODERULE.DBF")
			Select Name, Type, Script, UniqueID From This.cRuleDir + "CODERULE" Where Active Into Array This.aRules
			Local lni

			For m.lni = 1 To Alen(This.aRules, 1)
				If Empty(This.aRules(m.lni, 1))
			Loop
				Endif

				If This.aRules(lni, 2) = "L"
					This.lLineRules = .T. && Needed for performance checks
			Exit
				Endif
			Endfor
		Endif
	Endproc

	Procedure ResetArrays
		Dimension This.aCode(1,4)
		This.aCode(1,1) = ""

		Dimension This.aWarnings(1,3)
		This.aWarnings(1,1) = ""
		This.aWarnings(1,2) = ""
		This.aWarnings(1,3) = ""
	Endproc

	Procedure Analyze
		*!******************************************************************************
		*!* ����.......: Analyze
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ���������������
		*!* �﷨.......:
		*!* ����.......: tcFile		�ַ��͡���Ҫ�������ļ�
		*!* ����ֵ.....:
		*!******************************************************************************
		Lparameters tcFile

		This.cMessage	= ""
		This.cError		= ""

		Local lcDir
		m.lcDir = Curdir()

		This.cMainProgram = m.tcFile

		If Not Empty(m.tcFile)
			If Not File(m.tcFile) And Not Used(m.tcFile)
				Messagebox("�ļ� " + m.tcFile + " �����ڡ�", 16, "�������")
				Return
			Endif
		Endif

		If Isnull(This.oTherm)
			This.oTherm = Newobject("cprogressform", "foxref.vcx", This.cHomeDir + "ANALYST.APP")
			This.oTherm.SetMax(100)
		Endif

		Local lc
		This.ResetArrays()

		Local lAlias
		m.lAlias = .F.

		If Empty(m.tcFile)	
			If Aselobj(la, 1) = 0
				** Should we use the Active project or not?
				*!* IF TYPE("_VFP.ActiveProject")="U"

				m.tcFile = Getfile("PRG;PJX;VCX;SCX", "ѡ���ļ�", "��", 0, "ѡ�����ڷ������ļ�")

				*!*	ELSE
				*!*		tcFile = _VFP.ActiveProject.Name
				*!*	ENDIF

				If Empty(m.tcFile)
					Return
				Endif

				Set Default To (Strtran(m.tcFile, Justfname(m.tcFile)))

				This.cHomeDir = (Strtran(m.tcFile, Justfname(m.tcFile)))
			Endif

		Else
			If Used(m.tcFile)
				m.lAlias = .T.

			Else
				Set Default To (Strtran(m.tcFile, Justfname(m.tcFile)))
				This.cHomeDir = (Strtran(m.tcFile, Justfname(m.tcFile)))
			Endif
		Endif

		Try
			This.csetesc		= On("ESCAPE")
			This.csetEscLabel	= On("KEY", "ESC")

			On Escape _Screen._Analyst.StopAnalysis()
			On Key Label Escape _Screen.StopAnalysis = .T.

			This.PreValidate()

			If Empty(m.tcFile)
				m.lc = "��ǰ����"

				This.cFile			= m.lc
				This.cMainProgram	= m.lc

				This.oTherm.SetDescription("���ڷ�����" + m.lc)
				This.oTherm.SetProgress(1)

				DoEvents

				This.oTherm.Show()
				This.AddToCursor(la(1).Name, la(1).Name, '', "����")

				This.AnalyzeCurrObj()

			Else
				m.lc = m.tcFile

				If Empty(This.cMainProgram)
					This.cMainProgram = m.lc
				Endif

				This.cFile = m.lc

				This.oTherm.SetDescription("���ڷ�����" + m.lc)
				This.oTherm.SetProgress(1)

				DoEvents

				This.oTherm.Show()
				This.AddToCursor(m.lc, m.lc, '', "�ļ�")
				This.AnalFile(m.tcFile, m.lAlias)
			Endif

			This.oTherm.Hide()


			If This.lDisplayForm
				If Not This.lProjectRun Or Reccount(This.cAnalysisCursor) > 0
					If Not Pemstatus(_Screen, "_analysisform", 5)
						_Screen.AddProperty("_analysisform", .Null.)
					Endif

					If Isnull(_Screen._Analysisform)
						Select 0
						Do Form codeanalresults Name _Screen._Analysisform
					Else
						_Screen._Analysisform.Refresh()
					Endif
				Endif
			Endif
		Catch To m.loErr
			If Not Isnull(This.oTherm)
				This.oTherm.Hide()
			Endif

			Error m.loErr.Procedure + " �еĵ� " + Transform(m.loErr.Lineno) + " �з�������" + m.loErr.Message
		Endtry

		This.PostValidate()

		Set Default To (m.lcDir)

		Local lc
		m.lc = This.csetesc

		On Escape &lc
		m.lc = This.cSetEscLabel

		On Key Label Escape &lc
	EndProc

	Procedure StopAnalysis
		Local lc
		If Pemstatus(_Screen, "StopAnalysis", 5)
			_Screen.StopAnalysis = .T.
		Endif
		
		m.lc = This.csetesc
		
		On Escape &lc
		
		This.oTherm.Hide()
		Cancel

	Procedure AnalyzeCurrObj
		*!******************************************************************************
		*!* ����.......: AnalyzeCurrObj
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ������ǰ����ķ���
		*!* �﷨.......:
		*!* ����.......: tlWork		�߼��͡�
		*!* ����ֵ.....:
		*!******************************************************************************
		Lparameters tlWork

		This.cFile = "��ǰ����" + la(1).Name

		Dimension This.aCode(1, 4)
		This.aCode(1, 1) = ""

		This.analyzeObj(la(1))

		m.lc = "�������" + Chr(13) + Chr(10)

		If Not Empty(This.aCode(1, 1))
			=Asort(This.aCode, 2)
		Endif

		m.lc = m.lc + Chr(13) + Chr(10) + "--- ������� ---" + Chr(13) + Chr(10)

		Local llTitle
		m.llTitle = .F.

		For m.lni = 1 To Alen(This.aCode, 1)
			If Empty(This.aCode(m.lni, 1))
		Loop
			Endif

			If This.aCode(m.lni, 2) > 40 And Not m.llTitle
				m.llTitle = .T.
				m.lc = m.lc + Chr(13) + Chr(10) + "--- ���ܵ�ѡ�� ---" + Chr(13) + Chr(10)
			Endif

			If Not m.tlWork  Or (m.tlWork And ;
					(This.aCode(m.lni, 2) > 40 Or ;
					(This.aCode(m.lni, 3) < (Max(1, This.aCode(m.lni, 2) / 2)) And Not ;
					THIS.aCode(m.lni, 3) = This.aCode(m.lni, 2))))

			Else
				This.aCode(m.lni, 1) = "ɾ��"
			Endif
		Endfor

		m.ln	= Alen(This.aCode, 1)
		m.ln2	= 1

		For m.lni = 1 To m.ln
			If Not Empty(This.aCode(m.lni, 1))
				If This.aCode(lni,1) = "ɾ��"
					=Adel(This.aCode, m.lni)
					m.lni = m.lni - 1
		Loop
				Else
					m.ln2 = m.ln2 + 1
				Endif
			Endif
		Endfor

		Dimension This.aCode(m.ln2, 4)

		If Empty(This.aCode(m.ln2, 1))
			This.aCode(ln2, 1) = ""
		Endif

		Return
	EndProc

	Procedure AnalyzeObj
		*!******************************************************************************
		*!* ����.......: AnalyzeObj
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ��������ķ���
		*!* �﷨.......:
		*!* ����.......: toObj ��Ҫ�����Ķ���
		*!* ����ֵ.....: ��
		*!******************************************************************************
		Lparameters toObj

		Local lni
		Local lcText
		m.lcText = ""

		Local loObj

		Local la(1)
		Local lnMethods
		This.oTherm.setstatus("����" + m.toObj.Name)

		m.lnMethods = Amembers(m.la, m.toObj, 1)

		This.AddToCursor(m.toObj.Name, m.toObj.Name, '', "����")

		If Not Pemstatus(m.toObj, "Name", 5)
			Return
		Endif

		For m.lni = 1 To m.lnMethods
			If m.la(m.lni, 2) = "Method" Or m.la(m.lni, 2) = "Event"
				If Pemstatus(m.toObj, "ReadMethod", 5)
					m.lcContent = m.toObj.ReadMethod(m.la(m.lni, 1))

					If Not Empty(m.lcContent)
						&& �����Ƿ���Ҫ���ݶ�һ�������������������Ĵ����� ���� ���� �¼����¼�����û����ֵ��
						This.Add2Array(m.toObj.Name + "." + m.la(m.lni, 1), Alines(laX, m.lcContent), @laX)
						*!* lcText = lcText + toobj.Name+"."+la(lni,1) + " - "+LTRIM(STR(ALINES(laX,lcCOntent)))+CHR(13)+CHR(10)
					Endif
				Endif
			Endif
		Endfor

		This.ValidateObject(m.toObj)

		For m.lni = 1 To m.lnMethods
			If m.la(m.lni, 2) = "Object"
				m.loObj = m.toObj.&la(m.lni, 1)
			
				This.analyzeObj(m.loObj)
			Endif
		Endfor
	EndProc

	Procedure AnalyzeCode
		Lparameters tcFile,tlWork
		If Empty(m.tcFile)
			m.tcFile = Getfile("PRG")
		Endif

		Dimension This.aCode(1,4)
		This.aCode(1,1) = ""
		This.aCode(1,2) = 0
		This.aCode(1,3) = 0
		This.aCode(1,4) = ""

		Local lni

		If Memlines(m.tcFile) > 1
			This.analstring(m.tcFile)

		Else
			This.AnalFile(m.tcFile)
		Endif

		m.lc = "�������" + Chr(13) + Chr(10)

		= Asort(This.aCode, 2)
	Endproc

	Procedure ScanSCXVCX
		Lparameters tcFile
		This.cFile = m.tcFile

		Local lnArea
		m.lnArea = Select()

		Local lcAlias
		m.lcAlias = Sys(2015)

		Select 0
		Use (m.tcFile) Again Shared Alias &lcAlias

		Scan For Not Empty(methods)	And !Deleted()
			This.cObject	= Trim(Parent)+Iif(Empty(Parent),"",".")+Trim(objname)
			This.cClassName = objname

			This.analstring(methods, tcFile)

			This.cObject	= ""
			This.cClassName	= ""
		Endscan

		Select (lcAlias)
		Use
		Select (lnArea)
	Endproc

	Procedure ScanMNX
		Lparameters tcFile
		This.cFile = m.tcFile

		Local lnArea
		m.lnArea = Select()

		Local lnArea,lcAlias
		m.lcAlias = Sys(2015)

		Select 0
		Use (m.tcFile) Again Shared Alias &lcAlias

		Scan
			If Not Empty(Setup)
				This.analstring(Setup, m.tcFile + " Setup")
			Endif

			If Not Empty(Procedure)
				This.analstring(Procedure, m.tcFile + " Procedures")
			Endif

			If Not Empty(cleanup)
				This.analstring(cleanup, m.tcFile + " Cleanup")
			Endif
		Endscan

		Select (lcAlias)
		Use
		Select (lnArea)
	Endproc

	Procedure BuildFakeProject
		*!******************************************************************************
		*!* ����.......: BuildFakeProject
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: Copyright (c) 2020 FarleyDimon Inc.
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ����һ�����ٵġ���Ŀ����
		*!* �﷨.......:
		*!* ����.......: tcDir	�ַ��ͣ�Ŀ¼��
		*!* ����ֵ.....: ��
		*!******************************************************************************
		Lparameters tcDir

		Local lnArea
		m.lnArea = Select()

		Local lcFile
		m.lcFile = Sys(2015)

		This.cFile = m.lcFile

		Select 0
		Create Cursor (lcFile) (Type C(1), Name M)

		This.AddToProj(m.tcDir, m.lcFile)
		Select (m.lnArea)
	Endproc

	Function GetFileExtensionType
		Lparameters tcExt
		Local lcRet
		m.lcRet = "X"

		Do Case
			Case m.tcExt = "PRG"
				m.lcRet = "P"
		Endcase

		Return m.lcRet
	Endfunc

	Procedure AddToProj
		*!******************************************************************************
		*!* ����.......: AddToProj
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: 
		*!* �﷨.......:
		*!* ����.......: tcDir		�ַ���,Ŀ¼��
		*!*              tcAlias	�ַ��ͣ��洢Ŀ¼�£������������¼�Ŀ¼�����ļ���Ϣ�� Cursor ��
		*!* ����ֵ.....: ��
		*!******************************************************************************
		Lparameters tcDir,tcAlias

		Local lnFiles
		Local lnDirs
		Local la(1)
		Local lni
		Local lcExt

		If Right(m.tcDir, 1) = "\"
			m.tcDir = Left(m.tcDir, Len(m.tcDir) - 1)
		Endif

		m.lnFiles = Adir(la, m.tcDir + "\*.*")

		For m.lni = 1 To m.lnFiles
			If Left(m.la(m.lni, 1), 1) <> "."
				m.lcExt = Justext(m.la(m.lni, 1))

				If Inlist(m.lcExt, "PRG", "VCX", "MNX", "FRX", "SCX")
					Insert Into (m.tcAlias) Values ("P", Fullpath(m.tcDir + "\" + m.la(m.lni, 1)))
				Endif
			Endif
		Endfor

		*!* �ݹ��������
		m.lnDirs = Adir(la, m.tcDir + "\*.", "D")

		For m.lni = 1 To m.lnDirs
			If Left(m.la(m.lni, 1), 1) <> "."
				This.AddToProj(m.tcDir + "\" + m.la(m.lni, 1), m.tcAlias)
			Endif
		Endfor
	Endproc
	
	Procedure scanDirectory
		Lparameters tcFile

		Local lnArea,lcFile
		m.lnArea = Select()

		Local lnArea,lcAlias

		If Not Pemstatus(_Screen, "StopAnalysis", 5)
			_Screen.AddProperty("StopAnalysis", .F.)
		Endif

		_Screen.StopAnalysis = .F.

		Select (m.tcFile)
		m.lcAlias = Alias()

		Scan For Not Type="H"
			If _Screen.StopAnalysis
		Exit
			Endif

			This.oTherm.SetProgress(Recno() / Reccount() * 95)

			m.lcFile = Strtran(Name, Chr(0))
			This.AnalFile(m.lcFile)
		Endscan

		Select (m.lcAlias)
		Use
		Select (m.lnArea)
	Endproc

	Procedure scanProject
		Lparameters tcFile
		This.cFile = m.tcFile

		Local lnArea,lcFile
		m.lnArea = Select()

		Local lnArea,lcAlias
		m.lcAlias = Sys(2015)

		Select 0
		Use (tcFile) Again Shared Alias &lcAlias

		Scan For Not Type="H"
			This.oTherm.SetProgress(Recno() / Reccount() * 95)

			m.lcFile = Strtran(Name,Chr(0))
			This.AnalFile(m.lcFile)
		Endscan

		Select (m.lcAlias)
		Use
		Select (m.lnArea)
	Endproc

	Procedure AnalFile
		Lparameters tcFile,tlAlias

		If Pcount() = 1
			m.tlAlias = .F.
		Endif

		This.oTherm.SetDescription("���ڷ��� " + m.tcFile)
		This.cFile = m.tcFile

		If m.tlAlias
			This.ScanDirectory(m.tcFile)
		Else
			Local lcRet
			m.lcRet = ""

			Local lcExt
			m.lcExt = Upper(Justext(m.tcFile))

			Try
				Do Case
					Case m.lcExt = "PRG"
						m.lcRet = This.analstring(Filetostr(m.tcFile))

					Case m.lcExt = "SCX"
						m.lcRet = This.ScanSCXVCX(m.tcFile)

					Case m.lcExt = "MNX"
						m.lcRet = This.ScanMNX(m.tcFile)

					Case m.lcExt = "FRX"
						m.lcRet = ""

					Case m.lcExt="ZIP"
						m.lcRet = "ѹ���ļ�-�Ѻ���"

					Case m.lcExt = "BAK"
						m.lcRet = "�Ѻ���"

					Case m.lcExt = "APP"
						m.lcRet = ""

					Case m.lcExt = "FLL"
						m.lcRet = ""

					Case m.lcExt = "DBF"
						m.lcRet = ""

					Case m.lcExt = "PJX"
						m.lcRet = This.scanProject(m.tcFile)

					Case m.lcExt = "VCX"
						m.lcRet = This.ScanSCXVCX(m.tcFile)

					Case Inlist(m.lcExt,"BMP","TXT","MSK","INC","H","JPG","GIF","ICO","SCT","MNT","FPT","TBK","PJT","VCT","FRA","SCA","MNA","VCA","XML","HTM","FXP")

					Otherwise
						Local lc

						Try
							m.lc = This.analstring(Filetostr(m.tcFile))
						Catch
							m.lc = ""
						Endtry

						m.lcRet = m.lc
				Endcase

			Catch To m.loErr
				m.lcRet = "��ȡ�ļ� " + m.tcFile +" ����: " + m.loErr.Message
			Endtry

			Return m.lcRet
		Endif

	Procedure analstring
		** Takes a piece of code and looks for any breaks in it.
		** if someone wanted to write a rule to analyze entire pieces of code, here is where
		** it would go.
		** so the first rule is to break it into individual lines and analyze them.

		Lparameters tcString,tcName

		Local la(1)
		Local laBreak(3)
		laBreak(1) = "PROCEDURE"
		laBreak(2) = "FUNCTION"
		laBreak(3) = "DEFINE"

		Local lnTotal
		m.lnTotal = Alines(la, m.tcString)

		Local lni

		Local lcText
		m.lcText = ""

		Local lcFunc,lcWord
		Local lnCount
		m.lnCount = 0

		Local laX(1)
		laX(1)= ""
		m.lcFunc = "Program" && JUSTFNAME(tcFile)

		For m.lni = 1 To m.lnTotal
			This.oTherm.setstatus("�� " + Ltrim(Str(m.lnTotal)) + " �е� "+Ltrim(Str(m.lni))+ " ��" )

			m.lcText = m.la(m.lni)
			*% Don't uppercase - some rules may need to be case sensitive
			*% lcText = ALLTRIM(UPPER(STRTRAN(lcText,"	")))
			m.lcText = Alltrim(Strtran(m.lcText, "	"))

			If Empty(m.lcText)
		Loop
			Endif

			*!* THIS.ValidateLine(lcText)
			m.lcWord = Left(m.lcText, Atc(" ", m.lcText) - 1)

			If Empty(m.lcWord) Or Ascan(laBreak, Upper(m.lcWord)) = 0
				m.lnCount = m.lnCount + 1
				Dimension laX(Iif(Empty(laX(1)), 1 ,Alen(laX,1) + 1))

				m.laX(Alen(m.laX, 1)) = m.lcText

			Else
				This.Add2Array(m.lcFunc, m.lnCount, @laX)

				m.lnCount = 0
				
				If m.lcText = "DEFINE CLASS"
				*If "DEFINE CLASS" $ Upper(m.lcText)
					
					m.lcFunc = Alltrim(Strtran(Upper(m.lcText), "DEFINE CLASS"))
					m.lcFunc = Proper(Left(m.lcFunc,Atc(" ", m.lcFunc) - 1))

					This.cObject = m.lcFunc
				Else
					m.lcFunc = Alltrim(Strtran(m.lcText, m.lcWord))
				Endif

				Dimension laX(1)
				laX(1) = ""
			Endif
		Endfor

		This.Add2Array(m.lcFunc, m.lnCount, @laX)

		This.nFileLines = m.lnTotal

		This.AddToCursor(m.tcName, m.tcName, This.cClassName, "�ļ�")
		This.ValidateFile(m.tcString, m.tcName)
	Endproc

	Procedure ValidateObject
		Lparameters toObj
		Local lni,lcFunc
		This.oObject = m.toObj
		This.cObject = m.toObj.Name

		For m.lni = 1 To Alen(This.aRules, 1)
			If Empty(This.aRules(m.lni, 1))
		Loop
			Endif

			If Alen(This.aRules, 2) > 3
				This.cWarningID = This.aRules(m.lni, 4)

			Else
				This.cWarningID = ""
			Endif

			If This.aRules(m.lni, 2) = "O"
				m.lcFunc = This.aRules(m.lni,3)
				Execscript(m.lcFunc)
			Endif
		Endfor
	Endproc

	Procedure ValidateLine
		*!******************************************************************************
		*!* ����.......: ValidateLine
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ����������
		*!* �﷨.......:
		*!* ����.......: tcLine		�ַ��͡��������Ĵ�����
		*!* ����ֵ.....: ��
		*!******************************************************************************
		Lparameters tcLine

		Local lni, lcFunc

		This.cLine = m.tcLine

		For m.lni = 1 To Alen(This.aRules, 1)
			If Empty(This.aRules(m.lni, 1))
		Loop
			Endif

			If Alen(This.aRules, 2) > 3
				This.cWarningID = This.aRules(m.lni, 4)
			Else
				This.cWarningID = ""

			Endif

			If This.aRules(m.lni, 2) = "L"
				m.lcFunc = This.aRules(m.lni, 3)
				Execscript(m.lcFunc)
			Endif
		Endfor
	EndProc

	Procedure ValidateFile
		Lparameters tcFile,tcName

		Local lni,lcFunc
		This.cFile = m.tcName
		This.cCode = m.tcFile

		For m.lni = 1 To Alen(This.aRules, 1)
			If Empty(This.aRules(m.lni, 1))
		Loop
			Endif

			If Alen(This.aRules, 2) > 3
				This.cWarningID = This.aRules(m.lni, 4)

			Else
				This.cWarningID = ""
			Endif

			If This.aRules(m.lni, 2) = "F"
				m.lcFunc = This.aRules(m.lni, 3)
				Execscript(m.lcFunc)
			Endif
		Endfor

	Procedure ValidateCode
		Lparameters tcCode,tcName

		This.cFuncName = m.tcName

		If Empty(tcName) Or tcName = "Program"
			This.cFuncName = This.cFile
		Endif

		This.oTherm.setstatus("����/������" + m.tcName)

		Local lni,lcFunc
		This.cCode = m.tcCode

		For m.lni = 1 To Alen(This.aRules, 1)
			If Empty(This.aRules(m.lni, 1))
		Loop
			Endif

			If Alen(This.aRules, 2) > 3
				This.cWarningID = This.aRules(m.lni, 4)

			Else
				This.cWarningID = ""
			Endif

			If This.aRules(m.lni, 2) = "M"
				m.lcRule = This.aRules(m.lni, 1)
				m.lcFunc = This.aRules(m.lni, 3)

				Try
					Execscript(m.lcFunc)

				Catch To m.loErr
					This.AddError(m.loErr, m.lcRule, m.lcFunc)
					This.aRules(m.lni,1) = ""
				Endtry
			Endif
		Endfor


	Procedure PreValidate
		*!******************************************************************************
		*!* ����.......: PreValidate
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ����ʽ��ʼ�������ǰ��Ԥ����
		*!* �﷨.......:
		*!* ����.......:
		*!* ����ֵ.....: ��
		*!******************************************************************************
		This.oTherm.setstatus("Ԥ����")

		Local lni,lcFunc

		For m.lni = 1 To Alen(This.aRules, 1)
			If Empty(This.aRules(m.lni, 1))
		Loop
			Endif

			If Alen(This.aRules, 2) > 3
				This.cWarningID = This.aRules(m.lni, 4)
			Else
				This.cWarningID = ""
			Endif

			If This.aRules(m.lni, 2) = "I"
				m.lcRule = This.aRules(m.lni, 1)
				m.lcFunc = This.aRules(m.lni, 3)

				Try
					Execscript(m.lcFunc)

				Catch To m.loErr
					This.AddError(m.loErr, m.lcRule, m.lcFunc)
					This.aRules(m.lni, 1) = ""
				Endtry
			Endif
		Endfor
	EndProc

	Procedure PostValidate
		*!******************************************************************************
		*!* ����.......: PostValidate
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: �ڴ������ִ������ɨβ��
		*!* �﷨.......:
		*!* ����.......:
		*!* ����ֵ.....: ��
		*!******************************************************************************
		This.oTherm.setstatus("���ڷ���")
		Local lni,lcFunc

		For m.lni = 1 To Alen(This.aRules, 1)
			If Empty(This.aRules(m.lni, 1))
		Loop
			Endif

			If Alen(This.aRules, 2) > 3
				This.cWarningID = This.aRules(m.lni, 4)
			Else
				This.cWarningID = ""
			Endif

			If This.aRules(m.lni,2) = "P"
				m.lcRule = This.aRules(m.lni, 1)
				m.lcFunc = This.aRules(m.lni, 3)

				Try
					Execscript(m.lcFunc)

				Catch To m.loErr
					This.AddError(m.loErr, m.lcRule, m.lcFunc)
					This.aRules(m.lni, 1) = ""
				Endtry
			Endif
		Endfor
	EndProc

	Procedure AddError
		Lparameters toErr,tcRule,tcFunc
		This.cError = This.cError + m.loErr.Message + " occurred on line "+Ltrim(Str(m.loErr.Lineno))+" ("+m.loErr.LineContents+") in rule "+m.tcRule + Chr(13)+Chr(10)

	Procedure Add2Array
		*!******************************************************************************
		*!* ����.......: Add2Array
		*!* ����.......: 
		*!* ����.......: 
		*!* ��Ȩ.......: 
		*!* ����汾...: Visual FoxPro09.00.0000.7423
		*!* ˵��.......: ������洢�������в����з���
		*!* �﷨.......:
		*!* ����.......: tcCode		�ַ��͡��������������¼���
		*!*              tnLines	��ֵ�͡���������
		*!*              taArray	���飬�����ô���
		*!* ����ֵ.....:
		*!******************************************************************************
		Lparameters tcCode, tnLines, taArray

		If Not Empty(This.aCode(1,1))
			Dimension This.aCode(Alen(This.aCode, 1) + 1, 4)
		Endif

		This.cFuncName = m.tcCode

		If Not Empty(This.cObject)
			This.cFuncName = Lower(This.cObject + "." + This.cFuncName)

		Else
			If Empty(m.tcCode) Or m.tcCode = "Program"
				This.cFuncName = This.cFile
			Endif
		Endif

		This.aCode(Alen(This.aCode, 1), 1) = This.cFuncName
		This.aCode(Alen(This.aCode, 1), 2) = m.tnLines
		This.aCode(Alen(This.aCode, 1), 4) = This.cClassName

		If Empty(This.aCode(Alen(This.aCode, 1), 1))
			This.aCode(Alen(This.aCode, 1), 1) = ""
		Endif

		Local lcCode
		m.lcCode = ""
		Local lni
		m.lnReal=0

		For m.lni = 1 To Alen(m.taArray, 1)
			If This.lLineRules
				If Alltrim(Strtran(m.taArray(m.lni), "	")) <> "*"
					If Not Empty(m.taArray(m.lni))
						This.nLine = m.lni

						This.AddToCursor(This.cFile, This.cFuncName, This.cClassName, "����/����")
						This.ValidateLine(m.taArray(m.lni))

						lnReal = lnReal+1
					Endif
				Endif
			Endif

			m.lcCode = m.lcCode + m.taArray(m.lni) + Chr(13) + Chr(10)
		Endfor

		This.aCode(Alen(This.aCode, 1), 3) = m.lnReal
		This.nFuncLines = m.tnLines

		This.AddToCursor(This.cFile, This.cFuncName, This.cClassName, "����/����")
		This.ValidateCode(m.lcCode, m.tcCode)
	Endproc
ENDDEFINE

*!* Ҳ�������ߵĲ��ԡ���������֮������
*!*	DEFINE CLASS frmResults AS FORM
*!*		DOCREATE = .T.
*!*		AUTOCENTER = .T.
*!*		CAPTION = "�ع����"
*!*		WINDOWTYPE = 1
*!*		WIDTH = 400
*!*		NAME = "Form1"


*!*		ADD OBJECT list1 AS LISTBOX WITH ;
*!*			COLUMNCOUNT = 3, ;
*!*			COLUMNWIDTHS = "250,50,50", ;
*!*			HEIGHT = 144, ;
*!*			LEFT = 24, ;
*!*			TOP = 60, ;
*!*			FONTSIZE=8,;
*!*			WIDTH = 374, ;
*!*			NAME = "List1"


*!*		ADD OBJECT command1 AS COMMANDBUTTON WITH ;
*!*			TOP = 216, ;
*!*			LEFT = 264, ;
*!*			HEIGHT = 27, ;
*!*			WIDTH = 84, ;
*!*			CAPTION = "\<OK", ;
*!*			NAME = "Command1"


*!*		ADD OBJECT label1 AS LABEL WITH ;
*!*			AUTOSIZE = .T., ;
*!*			CAPTION = "������", ;
*!*			HEIGHT = 17, ;
*!*			LEFT = 24, ;
*!*			TOP = 12, ;
*!*			WIDTH = 74, ;
*!*			NAME = "Label1"


*!*		ADD OBJECT label2 AS LABEL WITH ;
*!*			AUTOSIZE = .T., ;
*!*			CAPTION = "����", ;
*!*			HEIGHT = 17, ;
*!*			LEFT = 24, ;
*!*			TOP = 36, ;
*!*			WIDTH = 42, ;
*!*			NAME = "Label2"


*!*		ADD OBJECT label3 AS LABEL WITH ;
*!*			AUTOSIZE = .T., ;
*!*			CAPTION = "�к�", ;
*!*			HEIGHT = 17, ;
*!*			LEFT = 228, ;
*!*			TOP = 36, ;
*!*			WIDTH = 43, ;
*!*			NAME = "Label3"


*!*		ADD OBJECT label4 AS LABEL WITH ;
*!*			AUTOSIZE = .T., ;
*!*			CAPTION = "������", ;
*!*			HEIGHT = 17, ;
*!*			LEFT = 288, ;
*!*			TOP = 36, ;
*!*			WIDTH = 42, ;
*!*			NAME = "Label4"


*!*		PROCEDURE INIT
*!*			LPARAMETERS tcObj, taArray,tlWork

*!*			IF EMPTY(tlWork)
*!*				THIS.CAPTION = "�������"
*!*			ELSE
*!*				THIS.CAPTION = "�����������"
*!*			ENDIF

*!*			THIS.label1.CAPTION = tcObj

*!*			LOCAL lni,llTitle
*!*			llTitle = .F.
*!*			FOR lni =1 TO ALEN(_SCREEN._Analyst.aWarnings,1)
*!*				WITH THIS.list1
*!*					IF NOT EMPTY(_SCREEN._Analyst.aWarnings(lni,1))
*!*						IF NOT llTitle
*!*							.ADDITEM("*** ���� ***")
*!*							llTitle = .T.
*!*						ENDIF
*!*						.ADDITEM(_SCREEN._Analyst.aWarnings(lni,1))
*!*					ENDIF
*!*				ENDWITH
*!*			ENDFOR
*!*			THIS.list1.COLUMNCOUNT=1

*!*			IF .F.
*!*				LOCAL lni
*!*				FOR lni =1 TO ALEN(taArray,1)
*!*					WITH THIS.list1
*!*						IF NOT EMPTY(taArray(lni,1))
*!*							.ADDITEM(taArray(lni,1))
*!*							.LIST(.LISTCOUNT,2) = LTRIM(STR(taArray(lni,2)))
*!*							.LIST(.LISTCOUNT,3) = LTRIM(STR(taArray(lni,3)))
*!*						ENDIF
*!*					ENDWITH
*!*				ENDFOR
*!*			ENDIF
*!*			THIS.list1.LISTINDEX = 1
*!*		ENDPROC

*!*		PROCEDURE command1.CLICK
*!*			THISFORM.RELEASE()
*!*		ENDPROC
*!*	ENDDEFINE
