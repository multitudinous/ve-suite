// Machine generated IDispatch wrapper class(es) created with ClassWizard
/////////////////////////////////////////////////////////////////////////////
// IHNode wrapper class
#ifndef HASPEN_H
#define HASPEN_H

#ifndef _AFXDLL
#define _AFXDLL
#endif

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdisp.h>        // MFC Automation classes
#include <afxdtctl.h>		// MFC support for Internet Explorer 4 Common Controls
#ifndef _AFX_NO_AFXCMN_SUPPORT
#include <afxcmn.h>			// MFC support for Windows Common Controls
#endif // _AFX_NO_AFXCMN_SUPPORT

class IHNode : public COleDispatchDriver
{
public:
	IHNode() {}		// Calls COleDispatchDriver default constructor
	IHNode(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHNode(const IHNode& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetApplication();
	LPDISPATCH GetParent();
	CString GetName(const VARIANT& force);
	void SetName(const VARIANT& force, LPCTSTR lpszNewValue);
	long GetDimension();
	short GetValueType();
	VARIANT GetValue(const VARIANT& force);
	void SetValue(const VARIANT& force, const VARIANT& newValue);
	void SetValueAndUnit(const VARIANT& Value, short unitcol, const VARIANT& force);
	void SetValueUnitAndBasis(const VARIANT& Value, short unitcol, LPCTSTR basis, const VARIANT& force);
	BOOL GetHasAttribute(short attrnumber);
	short GetAttributeType(short attrnumber);
	VARIANT GetAttributeValue(short attrnumber, const VARIANT& force);
	void SetAttributeValue(short attrnumber, const VARIANT& force, const VARIANT& newValue);
	BOOL GetHasClassAttribute(LPCTSTR classid);
	short GetClassAttributeType(LPCTSTR classid);
	VARIANT GetClassAttributeValue(LPCTSTR classid, const VARIANT& force);
	void SetClassAttributeValue(LPCTSTR classid, const VARIANT& force, const VARIANT& newValue);
	LPDISPATCH GetElements();
	void Delete();
	void RemoveAll();
	void AddClassAttribute(LPCTSTR classid, short type);
	void DeleteClassAttribute(LPCTSTR classid);
	void PrintUseful(short append, LPCTSTR filename);
	LPDISPATCH FindNode(LPCTSTR path);
	CString NextIncomplete(VARIANT* code);
	CString BrowseNext(short direction, short io);
	void Hide(LPCTSTR Name);
	void Reveal(LPCTSTR Name);
	void Clear();
	void NewChild(BSTR* Name);
	void RenameChild(BSTR* Name);
	CString GetUnitString();
	VARIANT GetValueForUnit(short unitrow, short unitcol);
	void Reconcile(long code);
	void NewID(BSTR* Name);
	void Export(LPCTSTR filename);
};
/////////////////////////////////////////////////////////////////////////////
// IHapp wrapper class

class IHapp : public COleDispatchDriver
{
public:
	IHapp() {}		// Calls COleDispatchDriver default constructor
	IHapp(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHapp(const IHapp& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetApplication();
	LPDISPATCH GetParent();
	CString GetFullName();
	CString GetName();
	BOOL GetVisible();
	void SetVisible(BOOL bNewValue);
	LPDISPATCH GetTree();
	void Save();
	void SaveAs(BSTR* filename, const VARIANT& overwrite);
	void Close(const VARIANT& reserved);
	LPDISPATCH GetChoose(short* flag);
	LPDISPATCH NewSelection(LPCTSTR Key);
	LPDISPATCH GetSelection(LPCTSTR Key);
	void DeleteSelection(LPCTSTR Key);
	void SaveSelection(LPCTSTR Key);
	void Reinit();
	LPDISPATCH CreateRouteTree(BSTR* propname, BSTR* routeid, BSTR* opsetid, short flag);
	LPDISPATCH GetEngine();
	void SaveLink(LPUNKNOWN pStrm, long format);
	void LoadLink(LPUNKNOWN pStrm, long format);
	void Activate();
	void SetCompat(long flag);
	void Reconcile(long code);
	void AdviseParent(long dAdviseType, long lParam);
	void Generate(LPCTSTR filename, long mode);
	void Readback(LPCTSTR filename, long mode);
	void SetParent(LPUNKNOWN pParentAdviseSink, long dwCookie);
	void WriteArchive2(BSTR* filename, long bSaveChildren);
	LPDISPATCH GetNew3(const VARIANT& filename, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode);
	VARIANT GetRestore2(LPCTSTR filename);
	void InitNew2(const VARIANT& notused, const VARIANT& notused2, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode);
	void InitFromFile2(BSTR* filename, const VARIANT& readonly, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode);
	void InitFromArchive2(BSTR* filename, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode);
	void Run2(const VARIANT& async);
	void InitFromTemplate2(BSTR* filename, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode);
	LPDISPATCH GetLibRef();
	long GetSuppressDialogs();
	void SetSuppressDialogs(long nNewValue);
	void Export(long reptype, LPCTSTR filename);
	void UIDisable(LPCTSTR Key);
	LPUNKNOWN GetEngineSimulation();
	LPUNKNOWN GetEngineServer();
	void InitFromXML(LPCTSTR argument);
};
/////////////////////////////////////////////////////////////////////////////
// IHSelection wrapper class

class IHSelection : public COleDispatchDriver
{
public:
	IHSelection() {}		// Calls COleDispatchDriver default constructor
	IHSelection(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHSelection(const IHSelection& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetApplication();
	LPDISPATCH GetParent();
	long GetCount();
	VARIANT GetItem(const VARIANT& loc_or_name);
	short GetItemType(const VARIANT& loc_or_name);
	CString GetKey();
	CString GetLabel(long location);
	void SetLabel(long location, LPCTSTR lpszNewValue);
	void Add(const VARIANT& loc_or_name, short type, const VARIANT& Item, LPDISPATCH owner, const VARIANT& scrollarea, const VARIANT& row, const VARIANT& col, const VARIANT& index, const VARIANT& xtwip, const VARIANT& ytwip);
	void Clear();
	void Remove(const VARIANT& loc_or_name);
	BOOL Find(const VARIANT& object);
	void Copy();
	void CopyWithFormat();
	BOOL Paste();
	BOOL PasteSpecial(const VARIANT& format, const VARIANT& link);
	void Lock(BOOL fLock);
	void ClearOwned(LPDISPATCH owner);
	BOOL CanPrint();
	VARIANT PreparePrinting();
};
/////////////////////////////////////////////////////////////////////////////
// IHAPEngine wrapper class

class IHAPEngine : public COleDispatchDriver
{
public:
	IHAPEngine() {}		// Calls COleDispatchDriver default constructor
	IHAPEngine(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHAPEngine(const IHAPEngine& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	BOOL GetRunControl();
	void SetRunControl(BOOL bNewValue);
	BOOL GetControlPanel();
	void SetControlPanel(BOOL bNewValue);
	BOOL GetReady();
	void Step();
	void Stop();
	void Reinit(const VARIANT& object_type, const VARIANT& object_id);
	void MoveTo(long object_type, const VARIANT& object_id);
	void ConnectionDialog();
	BOOL Host(long host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory);
	long GetHostCount();
	CString HostDescription(long host_type);
	void RunSettings();
	CString GetEngineFilesSettings(long file);
	void SetEngineFilesSettings(long file, LPCTSTR lpszNewValue);
	BOOL GetOptionSettings(long type);
	void SetOptionSettings(long type, BOOL bNewValue);
	void StopPoints();
	long GetStopPointCount();
	void GetStopPoint(long index, long* type, BSTR* object_id, long* before_or_after);
	void AddStopPoint(long type, LPCTSTR object_id, long before_or_after);
	void DeleteStopPoint(long index);
	void ClearStopPoints();
	BOOL GetIsRunning();
	void Run2(const VARIANT& async);
	void ExportReport(LPCTSTR filename, long contents, const VARIANT& object_id);
	void SynchronizeEO(const VARIANT& reserved);
	void ReinitializeEO(const VARIANT& reserved);
};
/////////////////////////////////////////////////////////////////////////////
// IHAPLibRef wrapper class

class IHAPLibRef : public COleDispatchDriver
{
public:
	IHAPLibRef() {}		// Calls COleDispatchDriver default constructor
	IHAPLibRef(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHAPLibRef(const IHAPLibRef& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	short GetCountLibs();
	CString GetLibraryName(short index);
	CString GetLibraryPath(short index);
	CString InsertLibrary(LPCTSTR path, short location);
	void RemoveLibrary(short location);
	void MoveLibrary(short fromloc, short toloc);
	CString GetCategoryName(short index);
	short GetCategorySelected(LPCTSTR Name);
	void SetCategorySelected(LPCTSTR Name, short nNewValue);
	short GetCategoryLocSelected(short index);
	void SetCategoryLocSelected(short index, short nNewValue);
	void MoveCategory(short fromloc, short toloc);
	void SetLibraryActive(LPCTSTR displayname);
};
/////////////////////////////////////////////////////////////////////////////
// IHNodeCol wrapper class

class IHNodeCol : public COleDispatchDriver
{
public:
	IHNodeCol() {}		// Calls COleDispatchDriver default constructor
	IHNodeCol(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHNodeCol(const IHNodeCol& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetApplication();
	LPDISPATCH GetParent();
	long GetCount();
	long GetRowCount(long Dimension);
	long GetDimension();
	BOOL GetIsNamedDimension(const VARIANT& Dimension);
	LPDISPATCH GetItem(const VARIANT& loc_or_name, const VARIANT& loc_or_name2, const VARIANT& loc_or_name3, const VARIANT& loc_or_name4, const VARIANT& loc_or_name5);
	CString GetItemName(long location, const VARIANT& Dimension, const VARIANT& force);
	void SetItemName(long location, const VARIANT& Dimension, const VARIANT& force, LPCTSTR lpszNewValue);
	CString GetLabel(long Dimension, long location, const VARIANT& force);
	void SetLabel(long Dimension, long location, const VARIANT& force, LPCTSTR lpszNewValue);
	LPDISPATCH GetLabelNode(long Dimension, long location, VARIANT* Label);
	long GetLabelLocation(LPCTSTR Label, long Dimension);
	VARIANT GetLabelAttribute(long Dimension, long location, short attrnum, const VARIANT& force);
	void SetLabelAttribute(long Dimension, long location, short attrnum, const VARIANT& force, const VARIANT& newValue);
	short GetLabelAttributeType(long Dimension, long location, short attrnum);
	LPDISPATCH Add(const VARIANT& loc_or_name, const VARIANT& loc_or_name2, const VARIANT& loc_or_name3, const VARIANT& loc_or_name4, const VARIANT& loc_or_name5);
	void Insert(LPDISPATCH element, const VARIANT& loc_or_name, const VARIANT& loc_or_name2, const VARIANT& loc_or_name3, const VARIANT& loc_or_name4, const VARIANT& loc_or_name5);
	LPDISPATCH Remove(const VARIANT& loc_or_name, const VARIANT& loc_or_name2, const VARIANT& loc_or_name3, const VARIANT& loc_or_name4, const VARIANT& loc_or_name5);
	void InsertRow(long Dimension, long location);
	void RemoveRow(long Dimension, long location);
	CString GetDimensionName(long Dimension);
	void Reorder(const VARIANT& loc_or_name, short dir);
};
/////////////////////////////////////////////////////////////////////////////
// IScrollAreaInfo wrapper class

class IScrollAreaInfo : public COleDispatchDriver
{
public:
	IScrollAreaInfo() {}		// Calls COleDispatchDriver default constructor
	IScrollAreaInfo(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IScrollAreaInfo(const IScrollAreaInfo& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetNodeAt(long row, long col, short index);
	void GetMaximum(long* pRow, short* pRowExt, long* pCol, short* pColExt);
};
/////////////////////////////////////////////////////////////////////////////
// IHPlotVal wrapper class

class IHPlotVal : public COleDispatchDriver
{
public:
	IHPlotVal() {}		// Calls COleDispatchDriver default constructor
	IHPlotVal(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHPlotVal(const IHPlotVal& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	void SetContext(LPDISPATCH newValue);
	LPDISPATCH GetContext();
	CString GetTitle();
	CString GetTitleInt();
	void SetTitle(LPCTSTR lpszNewValue);
	CString GetXAxisTitle();
	CString GetXAxisTitleInt();
	void SetXAxisTitle(LPCTSTR lpszNewValue);
	CString GetYAxisTitle(const VARIANT& nAxis);
	CString GetYAxisTitleInt(const VARIANT& nAxis);
	void SetYAxisTitle(const VARIANT& nAxis, LPCTSTR lpszNewValue);
	CString GetLabel(long index);
	CString GetLabelInt(long index);
	void SetLabel(long index, LPCTSTR lpszNewValue);
	long GetNPoints();
	void SetNPoints(long nNewValue);
	long GetNVariables();
	void SetNVariables(long nNewValue);
	long GetNXVariables();
	void SetNXVariables(long nNewValue);
	double GetXDataValue(long index, long datano);
	void SetXData(long index, long datano, short attr, const VARIANT& newValue);
	double GetYDataValue(long index, long datano);
	void SetYData(long index, long datano, short attr, const VARIANT& newValue);
	void GetXData(long index, long datano, short* attr, VARIANT* Value);
	void GetYData(long index, long datano, short* attr, VARIANT* Value);
	void Clear();
	BOOL GetSwapAxises();
	void SetSwapAxises(BOOL bNewValue);
	BOOL GetReverseXAxis();
	void SetReverseXAxis(BOOL bNewValue);
	BOOL GetAddTimeStamp();
	void SetAddTimeStamp(BOOL bNewValue);
	BOOL GetShowLegend();
	void SetShowLegend(BOOL bNewValue);
	long GetPlotType();
	void SetPlotType(long nNewValue);
	BOOL GetParametric();
	void SetParametric(BOOL bNewValue);
	CString GetZAxisTitle();
	CString GetZAxisTitleInt();
	void SetZAxisTitle(LPCTSTR lpszNewValue);
	BOOL GetLive();
	void SetLive(BOOL bNewValue);
	// method 'AddAnnotationText' not emitted because of invalid return type or parameter type
	long GetAnnotationCount();
	// method 'GetAnnotationText' not emitted because of invalid return type or parameter type
	short GetGridType();
	void SetGridType(short nNewValue);
	short GetMarkerSize();
	void SetMarkerSize(short nNewValue);
	BOOL GetShowDiagonalLine();
	void SetShowDiagonalLine(BOOL bNewValue);
	short GetLineStyle();
	void SetLineStyle(short nNewValue);
	BOOL GetXDataInteger(long index);
	BOOL GetShowZeroLine();
	void SetShowZeroLine(BOOL bNewValue);
	long GetAxisMap(long index);
	void SetAxisMap(long index, long nNewValue);
	long GetCurveStyle(long index);
	void SetCurveStyle(long index, long nNewValue);
	void RemoveAnnotationText();
	BOOL GetSquarePlot();
	void SetSquarePlot(BOOL bNewValue);
	BOOL GetCanLive();
	void SetCanLive(BOOL bNewValue);
	long GetAxisScale(long VarNo);
	void SetAxisScale(long VarNo, long nNewValue);
};
/////////////////////////////////////////////////////////////////////////////
// IAPPDF wrapper class

class IAPPDF : public COleDispatchDriver
{
public:
	IAPPDF() {}		// Calls COleDispatchDriver default constructor
	IAPPDF(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPPDF(const IAPPDF& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	void GetAPPDF(LPCTSTR filename, long flags);
};
/////////////////////////////////////////////////////////////////////////////
// IHAPHandle wrapper class

class IHAPHandle : public COleDispatchDriver
{
public:
	IHAPHandle() {}		// Calls COleDispatchDriver default constructor
	IHAPHandle(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHAPHandle(const IHAPHandle& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	long GetMainHWnd();
	long GetProcessHandle();
};
/////////////////////////////////////////////////////////////////////////////
// IAPropData wrapper class

class IAPropData : public COleDispatchDriver
{
public:
	IAPropData() {}		// Calls COleDispatchDriver default constructor
	IAPropData(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPropData(const IAPropData& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	void GetAvailableDatabanks(long* plNumDatabanks, VARIANT* pvtDatabankArray);
	void GetDefaultDatabanks(long* plNumDatabanks, VARIANT* pvtDatabankArray);
	void SetQueryForComponents(const VARIANT& vtDatabanksArray, LPCTSTR bstrMatchNameAlias, long bMatchAlternate, long bMatchStringBeginOnly, LPCTSTR bstrCompClass, LPCTSTR bstrCASRN, double dblMWfrom, double dblMWto, double dblBPfrom, 
		double dblBPto, LPCTSTR bstrBPUnit);
	void FetchNextComponent(long bInit, BSTR* bstrAlias, BSTR* bstrName, double* dblBP, double* dblMW, BSTR* bstrDatabank, BSTR* bstrCASRN, BSTR* bstrCompClass, BSTR* bstrSynonym);
};
/////////////////////////////////////////////////////////////////////////////
// IAPPasteItems wrapper class

class IAPPasteItems : public COleDispatchDriver
{
public:
	IAPPasteItems() {}		// Calls COleDispatchDriver default constructor
	IAPPasteItems(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPPasteItems(const IAPPasteItems& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetItem(const VARIANT& index);
	LPUNKNOWN GetEnum();
	long GetCount();
};
/////////////////////////////////////////////////////////////////////////////
// IAPPasteItem wrapper class

class IAPPasteItem : public COleDispatchDriver
{
public:
	IAPPasteItem() {}		// Calls COleDispatchDriver default constructor
	IAPPasteItem(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPPasteItem(const IAPPasteItem& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	CString GetDisplayname();
	void SetId(LPCTSTR lpszNewValue);
	CString GetId();
	void SetDeleted(BOOL bNewValue);
	BOOL GetDeleted();
	long GetPathLen();
	CString GetPath(long index);
	BOOL GetHasChildren();
	LPDISPATCH GetItems();
};
/////////////////////////////////////////////////////////////////////////////
// IAPHappEvents wrapper class

class IAPHappEvents : public COleDispatchDriver
{
public:
	IAPHappEvents() {}		// Calls COleDispatchDriver default constructor
	IAPHappEvents(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPHappEvents(const IAPHappEvents& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	void OnDialogSuppressed(LPCTSTR msg, LPCTSTR result);
	void OnControlPanelMessage(long Clear, LPCTSTR msg);
	void OnGUIClosing();
	void OnInClosing(long Clear, LPCTSTR msg);
	void OnDataChanged(LPDISPATCH pObj);
	void OnUpdateMessage(long msg_code, LPCTSTR msg_hint, LPDISPATCH msg_object, long source_handle);
	void OnEngineCommandCompleted();
	void OnBeforeCalculate(BOOL IsStep, BOOL* Cancel);
	void OnBeforeSave(LPCTSTR filename, BOOL* Cancel);
	void OnCalculationCompleted();
	void OnCalculationStopped();
};
/////////////////////////////////////////////////////////////////////////////
// IHComposite wrapper class

class IHComposite : public COleDispatchDriver
{
public:
	IHComposite() {}		// Calls COleDispatchDriver default constructor
	IHComposite(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHComposite(const IHComposite& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	void AddChild(LPCTSTR Name, LPDISPATCH child);
	void RemoveChild(LPCTSTR Name);
	LPDISPATCH GetChild(long index);
	CString GetChildName(long index);
	long GetChildCount();
	void DumpToDataBase(LPCTSTR filename, LPCTSTR tablename, long create);
};
/////////////////////////////////////////////////////////////////////////////
// IHWizardPlot wrapper class

class IHWizardPlot : public COleDispatchDriver
{
public:
	IHWizardPlot() {}		// Calls COleDispatchDriver default constructor
	IHWizardPlot(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHWizardPlot(const IHWizardPlot& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetPlotVal();
	long GetBasis();
	void SetBasis(long nNewValue);
	long GetUnitrow(const VARIANT& curve);
	void SetUnitrow(const VARIANT& curve, long nNewValue);
	long GetUnitcol(const VARIANT& curve);
	void SetUnitcol(const VARIANT& curve, long nNewValue);
	void SetIndependentVariable(LPDISPATCH pContext, LPCTSTR indepvar);
	void AddDependentVariable(LPDISPATCH pContext, LPCTSTR depvar);
	void RemoveDependentVariable(LPDISPATCH pContext, LPCTSTR depvar);
	void RemoveAllDependentVariable();
	BOOL GetHasDependentVariable(LPDISPATCH pContext, LPCTSTR depvar);
	void AddVariablePair(LPDISPATCH pContext, LPCTSTR indepvar, LPCTSTR depvar, short sort);
	LPDISPATCH GetWizardData(long Step);
	void SetWizardData(long Step, LPDISPATCH newValue);
	void Clear();
	long GetErrorNumber();
	void SetErrorStatus(long index, BOOL bNewValue);
	void SetRefreshCallBack(LPCTSTR lpszNewValue);
	CString GetRefreshCallBack();
	void GetIndependentVariable(BSTR* indepvar);
	// method 'GetDependentVariable' not emitted because of invalid return type or parameter type
	// method 'GetPlotList' not emitted because of invalid return type or parameter type
	void SetAddToPlot(LPDISPATCH newValue);
	LPDISPATCH GetAddToPlot();
	long GetXUnitRow();
	long GetXUnitCol();
	void SetXUnitCol(long nNewValue);
};
/////////////////////////////////////////////////////////////////////////////
// IHAdhocPlot wrapper class

class IHAdhocPlot : public COleDispatchDriver
{
public:
	IHAdhocPlot() {}		// Calls COleDispatchDriver default constructor
	IHAdhocPlot(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHAdhocPlot(const IHAdhocPlot& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetPlotVal();
	void ReadyToPlot();
	void SetIndependentVariable();
	void SetDependentVariable();
	void SetParametricVariable();
	void Refresh();
};
/////////////////////////////////////////////////////////////////////////////
// IMMControlVerb wrapper class

class IMMControlVerb : public COleDispatchDriver
{
public:
	IMMControlVerb() {}		// Calls COleDispatchDriver default constructor
	IMMControlVerb(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IMMControlVerb(const IMMControlVerb& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	void EnumVerbs(VARIANT* pEnumOleVerb);
	void DoVerb(long iVerb);
};
/////////////////////////////////////////////////////////////////////////////
// IHappServiceProvider wrapper class

class IHappServiceProvider : public COleDispatchDriver
{
public:
	IHappServiceProvider() {}		// Calls COleDispatchDriver default constructor
	IHappServiceProvider(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHappServiceProvider(const IHappServiceProvider& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPUNKNOWN GetService(LPCTSTR bService);
};
/////////////////////////////////////////////////////////////////////////////
// IAPConflict wrapper class

class IAPConflict : public COleDispatchDriver
{
public:
	IAPConflict() {}		// Calls COleDispatchDriver default constructor
	IAPConflict(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPConflict(const IAPConflict& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	CString GetDisplayname();
	void SetId(LPCTSTR lpszNewValue);
	CString GetId();
	void SetMerge(BOOL bNewValue);
	BOOL GetMerge();
	void SetAction(long nNewValue);
	long GetAction();
};
/////////////////////////////////////////////////////////////////////////////
// IAPConflicts wrapper class

class IAPConflicts : public COleDispatchDriver
{
public:
	IAPConflicts() {}		// Calls COleDispatchDriver default constructor
	IAPConflicts(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPConflicts(const IAPConflicts& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH Item(const VARIANT& index);
	LPUNKNOWN GetEnum();
	void Clear();
	long GetCount();
};
/////////////////////////////////////////////////////////////////////////////
// IAPGroup wrapper class

class IAPGroup : public COleDispatchDriver
{
public:
	IAPGroup() {}		// Calls COleDispatchDriver default constructor
	IAPGroup(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPGroup(const IAPGroup& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	void Clear();
	void Add(const VARIANT& Item);
	void Remove(const VARIANT& Item);
	VARIANT Item(const VARIANT& index);
	LPUNKNOWN Get_NewEnum();
	long GetCount();
	LPUNKNOWN GetDataObject();
	void Export(LPCTSTR filename);
	void CopyToClipboard();
};
/////////////////////////////////////////////////////////////////////////////
// IAPPasteParam wrapper class

class IAPPasteParam : public COleDispatchDriver
{
public:
	IAPPasteParam() {}		// Calls COleDispatchDriver default constructor
	IAPPasteParam(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPPasteParam(const IAPPasteParam& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	CString GetId();
	void SetLabel(LPCTSTR lpszNewValue);
	CString GetLabel();
	void SetValue(LPCTSTR lpszNewValue);
	CString GetValue();
};
/////////////////////////////////////////////////////////////////////////////
// IAPPasteParams wrapper class

class IAPPasteParams : public COleDispatchDriver
{
public:
	IAPPasteParams() {}		// Calls COleDispatchDriver default constructor
	IAPPasteParams(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPPasteParams(const IAPPasteParams& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetItem(const VARIANT& index);
	LPUNKNOWN GetEnum();
	long GetCount();
};
/////////////////////////////////////////////////////////////////////////////
// IAPPasteBuffer wrapper class

class IAPPasteBuffer : public COleDispatchDriver
{
public:
	IAPPasteBuffer() {}		// Calls COleDispatchDriver default constructor
	IAPPasteBuffer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAPPasteBuffer(const IAPPasteBuffer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH GetItems();
	LPDISPATCH GetParams();
	LPDISPATCH GetConflicts();
	void SetContext(const VARIANT& newValue);
	void Import(LPCTSTR filename);
	void Export(LPCTSTR filename);
	void Commit(const VARIANT& force, const VARIANT& errorLog);
};
/////////////////////////////////////////////////////////////////////////////
// IHNodeSelection wrapper class

class IHNodeSelection : public COleDispatchDriver
{
public:
	IHNodeSelection() {}		// Calls COleDispatchDriver default constructor
	IHNodeSelection(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IHNodeSelection(const IHNodeSelection& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attributes
public:

// Operations
public:
	LPDISPATCH Select(LPCTSTR nodePath);
	VARIANT SelectNodes(LPCTSTR nodePath);
	VARIANT SelectValues(LPCTSTR nodePath);
	VARIANT SelectAttributes(LPCTSTR nodePath, const VARIANT& attributes);
	VARIANT SelectLabelAttributes(LPCTSTR nodePath, long Dimension, const VARIANT& attributes);
	VARIANT SelectSingleNodeAttributes(LPCTSTR nodePath, const VARIANT& attributes);
	VARIANT SelectAttributesOnNodes(const VARIANT& nodePathArray, const VARIANT& attributes);
};



typedef enum {
    HAP_VALUE = 0,
    HAP_RESERVED1 = 1,
    HAP_UNITROW = 2,
    HAP_UNITCOL = 3,
    HAP_RESERVED2 = 4,
    HAP_OPTIONLIST = 5,
    HAP_RECORDTYPE = 6,
    HAP_ENTERABLE = 7,
    HAP_UPPERLIMIT = 8,
    HAP_LOWERLIMIT = 9,
    HAP_VALUEDEFAULT = 10,
    HAP_USERENTERED = 11,
    HAP_COMPSTATUS = 12,
    HAP_BASIS = 13,
    HAP_INOUT = 14,
    HAP_PORTSEX = 15,
    HAP_MULTIPORT = 16,
    HAP_PORTTYPE = 17,
    HAP_OUTVAR = 18,
    HAP_PROMPT = 19,
    HAP_PRETENDNOTENTERED = 20,
    HAP_HELPFILENAME = 21,
    HAP_HELPID = 22,
    HAP_FIRSTPAIR = 23,
    HAP_NODENAME = 24,
    HAP_METHOD = 25,
    HAP_MARKED = 26,
    HAP_VOLATILE = 27,
    HAP_SECTION = 28,
    HAP_DEFNAME = 29,
    HAP_CANADD = 30,
    HAP_CANDELETE = 31,
    HAP_CANRENAME = 32,
    HAP_CANHIDE = 33,
    HAP_CANREVEAL = 34,
    HAP_CANCLEAR = 35,
    HAP_CANCOPY = 36,
    HAP_CANPASTE = 37,
    HAP_HASCHILDREN = 38,
    HAP_PLOTLABEL = 39,
    HAP_BIRDCAGE = 40,
    HAP_STREAMCLASS = 42,
    HAP_HASCOMMENTS = 43,
    HAP_CANHAVECOMMENTS = 44,
    HAP_UNDERLYINGPATH = 45,
    HAP_ISHIDDEN = 47,
    HAP_HIDEVIEW = 48,
    HAP_ANALYSISFLAG = 49,
    HAP_SPECSTREAM = 50,
    HAP_REORDER = 51,
    HAP_ISREALSYMBOL = 52,
    HAP_CANEXPORT = 58,
    HAP_BASETYPE = 59,
    HAP_HIERARCHYFLAG = 63,
    HAP_HIERPATH = 64,
    HAP_FULLNAME = 65,
    HAP_CANTEMPLAPPEND = 66,
    HAP_ACTIVATESTATE = 67,
    HAP_CANIMPORT = 68,
    HAP_HASEOMSG = 69,
    HAP_SHOWEOMSG = 70,
    HAP_EOEXPORT = 72,
    HAP_EOIMPORT = 73,
    HAP_NAVPATH = 74,
    HAP_HIERNAME = 76,
    HAP_REVEALLIST = 77,
    HAP_DEFRECONCILE = 78,
    HAP_EONODENAME = 79,
    HAP_UOM = 81,
    HAP_UOMSET = 82
} HAPAttributeNumber;


typedef enum {
    HAPP_ENABLE_DEACTIVATE = 0,
    HAPP_ENABLE_ACTIVATE = 1,
    HAPP_ENABLE_NOT_APPLICABLE = 2
} HAPP_ENABLE_CODE;

typedef enum
{
    IAP_REINIT_BLOCK = 1,
    IAP_REINIT_CONVERGENCE,
    IAP_REINIT_STREAM,
    IAP_REINIT_SIMULATION
}
IAP_REINIT_TYPE;

#endif