// Machine generated IDispatch wrapper class(es) created with ClassWizard
#ifndef HDYN_H
#define HDYN_H

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

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerPhysicalProperties wrapper class

class IAspenModelerPhysicalProperties : public COleDispatchDriver
{
public:
	IAspenModelerPhysicalProperties() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerPhysicalProperties(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerPhysicalProperties(const IAspenModelerPhysicalProperties& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetComponentListNames();
	void SetComponentListNames(const VARIANT&);
	VARIANT GetPropertiesFileName();
	void SetPropertiesFileName(const VARIANT&);

	// Operations
public:
	LPDISPATCH ComponentList(const VARIANT& sName);
	void AddComponentList(const VARIANT& sName, const VARIANT& sSet);
	void RemoveComponentList(const VARIANT& sName);
	void LoadPropertiesFile(const VARIANT& sPropertiesFilename);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerComponentList wrapper class

class IAspenModelerComponentList : public COleDispatchDriver
{
public:
	IAspenModelerComponentList() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerComponentList(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerComponentList(const IAspenModelerComponentList& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetComponents();
	void SetComponents(const VARIANT&);
	VARIANT GetOptionNames();
	void SetOptionNames(const VARIANT&);
	VARIANT GetOption();
	void SetOption(const VARIANT&);
	VARIANT GetName();
	void SetName(const VARIANT&);
	VARIANT GetIsComponentSet();
	void SetIsComponentSet(const VARIANT&);

	// Operations
public:
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerHomotopy wrapper class

class IAspenModelerHomotopy : public COleDispatchDriver
{
public:
	IAspenModelerHomotopy() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerHomotopy(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerHomotopy(const IAspenModelerHomotopy& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetCount();
	void SetCount(const VARIANT&);
	VARIANT GetHomotopyEnabled();
	void SetHomotopyEnabled(const VARIANT&);
	VARIANT GetTheta();
	void SetTheta(const VARIANT&);
	VARIANT GetVariable();
	void SetVariable(const VARIANT&);

	// Operations
public:
	void AddTarget(const VARIANT& sVarName, const VARIANT& sVarValue, const VARIANT& sVarUnits);
	VARIANT GetTarget(const VARIANT& sVarName, const VARIANT& sUnit);
	void RemoveAll();
	void SetTarget(const VARIANT& sVarName, const VARIANT& sTarget, const VARIANT& sUnit);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModeler wrapper class

class IAspenModeler : public COleDispatchDriver
{
public:
	IAspenModeler() {}		// Calls COleDispatchDriver default constructor
	IAspenModeler(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModeler(const IAspenModeler& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetApplication();
	void SetApplication(const VARIANT&);
	VARIANT GetAppName();
	void SetAppName(const VARIANT&);
	VARIANT GetOutputLogger();
	void SetOutputLogger(const VARIANT&);
	VARIANT GetFullName();
	void SetFullName(const VARIANT&);
	VARIANT GetName();
	void SetName(const VARIANT&);
	VARIANT GetVersion();
	void SetVersion(const VARIANT&);
	VARIANT GetVisible();
	void SetVisible(const VARIANT&);
	VARIANT GetPath();
	void SetPath(const VARIANT&);
	VARIANT GetStatusBar();
	void SetStatusBar(const VARIANT&);
	VARIANT GetCaption();
	void SetCaption(const VARIANT&);
	VARIANT GetDefaultFilePath();
	void SetDefaultFilePath(const VARIANT&);
	VARIANT GetInteractive();
	void SetInteractive(const VARIANT&);
	VARIANT GetSimulation();
	void SetSimulation(const VARIANT&);
	VARIANT GetHeight();
	void SetHeight(const VARIANT&);
	VARIANT GetLeft();
	void SetLeft(const VARIANT&);
	VARIANT GetTop();
	void SetTop(const VARIANT&);
	VARIANT GetWidth();
	void SetWidth(const VARIANT&);
	VARIANT GetWindow();
	void SetWindow(const VARIANT&);
	VARIANT GetProcessId();
	void SetProcessId(const VARIANT&);
	VARIANT GetParent();
	void SetParent(const VARIANT&);
	VARIANT GetConfiguration();
	void SetConfiguration(const VARIANT&);
	VARIANT GetActiveDocument();
	void SetActiveDocument(const VARIANT&);
	VARIANT GetWorkingFolder();
	void SetWorkingFolder(const VARIANT&);
	VARIANT GetProcessIDs();
	void SetProcessIDs(const VARIANT&);

	// Operations
public:
	VARIANT DefaultColor();
	void PrintToMessageWindow(const VARIANT& Message);
	void Quit();
	void Msg(const VARIANT& Message);
	VARIANT NewDocument();
	VARIANT OpenDocument(const VARIANT& vDocumentName);
	VARIANT CloseDocument(const VARIANT& saveChanges);
	void SaveDocument();
	void SaveDocumentAs(const VARIANT& vPathName, const VARIANT& vReplace);
	void Minimize();
	void Maximize();
	void Restore();
	void Activate();
	void ClearApplicationData();
	VARIANT AddEventSink(const VARIANT& pSink);
	void RemoveEventSink(const VARIANT& pSink);
	VARIANT GetApplicationData(const VARIANT& sName);
	void SetApplicationData(const VARIANT& sName, const VARIANT& newValue);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEvents wrapper class

class IAspenModelerEvents : public COleDispatchDriver
{
public:
	IAspenModelerEvents() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerEvents(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerEvents(const IAspenModelerEvents& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:

	// Operations
public:
	void OnRunStarted();
	void OnRunPaused();
	void OnHasQuit();
	void OnHasSaved();
	void OnSavedAs(const VARIANT& sPath);
	void OnOpened(const VARIANT& sPath);
	void OnNew();
	void OnRunModeChanged(const VARIANT& sRunMode);
	void OnNewBlock(const VARIANT& sBlockName);
	void OnNewStream(const VARIANT& sStreamName);
	void OnDeletedBlock(const VARIANT& sBlockName);
	void OnDeletedStream(const VARIANT& sStreamName);
	void OnUomSetChanged(const VARIANT& sUomSetName);
	void OnStreamConnected(const VARIANT& sStreamName);
	void OnUserChangedVariable(const VARIANT& sVariableName, const VARIANT& sAttributeName);
	void OnStreamDisconnected(const VARIANT& sStreamName);
	void OnStepComplete();
	void OnRewindorCopyValues();
	void OnUserEvent(const VARIANT& sUserString);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerDocument wrapper class

class IAspenModelerDocument : public COleDispatchDriver
{
public:
	IAspenModelerDocument() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerDocument(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerDocument(const IAspenModelerDocument& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetProblem();
	void SetProblem(const VARIANT&);
	VARIANT GetFlowsheet();
	void SetFlowsheet(const VARIANT&);
	VARIANT GetApplication();
	void SetApplication(const VARIANT&);
	VARIANT GetOutputLogger();
	void SetOutputLogger(const VARIANT&);
	VARIANT GetOptions();
	void SetOptions(const VARIANT&);
	VARIANT GetFullName();
	void SetFullName(const VARIANT&);
	VARIANT GetName();
	void SetName(const VARIANT&);
	VARIANT GetParent();
	void SetParent(const VARIANT&);
	VARIANT GetSaved();
	void SetSaved(const VARIANT&);
	VARIANT GetDegrees();
	void SetDegrees(const VARIANT&);
	VARIANT GetEquations();
	void SetEquations(const VARIANT&);
	VARIANT GetRunMode();
	void SetRunMode(const VARIANT&);
	VARIANT GetRunNumber();
	void SetRunNumber(const VARIANT&);
	VARIANT GetSpecState();
	void SetSpecState(const VARIANT&);
	VARIANT GetState();
	void SetState(const VARIANT&);
	VARIANT GetTermination();
	void SetTermination(const VARIANT&);
	VARIANT GetTime();
	void SetTime(const VARIANT&);
	VARIANT GetVariables();
	void SetVariables(const VARIANT&);
	VARIANT GetResults();
	void SetResults(const VARIANT&);
	VARIANT GetUom();
	void SetUom(const VARIANT&);
	VARIANT GetEndTime();
	void SetEndTime(const VARIANT&);
	VARIANT GetLeastSquaresObjective();
	void SetLeastSquaresObjective(const VARIANT&);
	VARIANT GetCovarianceMatrix();
	void SetCovarianceMatrix(const VARIANT&);
	VARIANT GetCorrelationMatrix();
	void SetCorrelationMatrix(const VARIANT&);
	VARIANT GetCorrelationMatrixPresent();
	void SetCorrelationMatrixPresent(const VARIANT&);
	VARIANT GetDeviationArrayPresent();
	void SetDeviationArrayPresent(const VARIANT&);
	VARIANT GetCommunicationInterval();
	void SetCommunicationInterval(const VARIANT&);
	VARIANT GetSuccessful();
	void SetSuccessful(const VARIANT&);
	long GetFlowsheetModified();
	void SetFlowsheetModified(long);
	VARIANT GetIsModified();
	void SetIsModified(const VARIANT&);
	VARIANT GetScriptIsRunning();
	void SetScriptIsRunning(const VARIANT&);
	VARIANT GetHomotopy();
	void SetHomotopy(const VARIANT&);
	VARIANT GetFixedHeteroscedasticity();
	void SetFixedHeteroscedasticity(const VARIANT&);
	VARIANT GetEstimationRunState();
	void SetEstimationRunState(const VARIANT&);
	VARIANT GetEndStepCount();
	void SetEndStepCount(const VARIANT&);
	VARIANT GetEstimatedVariables();
	void SetEstimatedVariables(const VARIANT&);
	VARIANT GetDefaultTimeUnit();
	void SetDefaultTimeUnit(const VARIANT&);
	VARIANT GetExperimentTimeUnit();
	void SetExperimentTimeUnit(const VARIANT&);
	VARIANT GetDisplayTime();
	void SetDisplayTime(const VARIANT&);
	VARIANT GetDisplayTimeUnit();
	void SetDisplayTimeUnit(const VARIANT&);
	VARIANT GetFlowsheetWallpaperMode();
	void SetFlowsheetWallpaperMode(const VARIANT&);
	VARIANT GetLastError();
	void SetLastError(const VARIANT&);
	VARIANT GetGetEstimationMatrixVariables();
	void SetGetEstimationMatrixVariables(const VARIANT&);

	// Operations
public:
	void Copy(const VARIANT& sPath);
	void PasteLink(const VARIANT& sPath);
	void ShowWindow(long bShow);
	long SetSolverProperty(LPCTSTR sSolverOptionName, const VARIANT& vSolverOptionValue);
	long GetSolverProperty(LPCTSTR sSolverOptionName, VARIANT* pSolverOptionValue);
	void Close(const VARIANT& saveChanges, const VARIANT& fileName);
	void LaunchFormView(const VARIANT& cvPlotPath, const VARIANT& cvx, const VARIANT& cvy, const VARIANT& cvcx, const VARIANT& cvcy, const VARIANT& cbHistory);
	VARIANT NewExperiment(const VARIANT& cvName, const VARIANT& cvRunMode);
	void AddExperiment(const VARIANT& cvExperiment);
	void AddEstimateVariable(const VARIANT& cvVariable);
	VARIANT GetEstimatedVariableValue(const VARIANT& varVariable);
	VARIANT GetEstimatedVariableDeviation(const VARIANT& varName);
	void ResetEstimation();
	void Run(const VARIANT& cvWait);
	void Step(const VARIANT& cvWait);
	void Pause();
	VARIANT GetStatusMessage(const VARIANT& cvStatus);
	void CreateLibrary(const VARIANT& cvLibraryName);
	void AddSensitivityParameter(const VARIANT& cvVarName);
	void AddSensitivityVariable(const VARIANT& cvVarName);
	void ClearSensitivities();
	void EnableSensitivities();
	void DisableSensitivities();
	VARIANT GetSensitivityValue(const VARIANT& cvVarName, const VARIANT& cvParmName);
	void Reset();
	void Restart();
	LPDISPATCH Optimization();
	void SaveDocument();
	void SaveDocumentAs(const VARIANT& vPathName, const VARIANT& vReplace);
	LPDISPATCH CDI();
	LPDISPATCH Properties();
	void Interrupt(const VARIANT& bWait);
	void LaunchAppView(const VARIANT& cvGUID);
	void ClearDocumentData();
	VARIANT GetCorrelation(const VARIANT& cvEstVar1, const VARIANT& cvEstVar2);
	VARIANT GetCovariance(const VARIANT& cvEstVar1, const VARIANT& cvEstVar2);
	VARIANT GetDeviation(const VARIANT& cvEstVar);
	VARIANT GetEstimatedValue(const VARIANT& cvEstVar);
	void SetDocumentData(const VARIANT& sName, const VARIANT& newValue);
	VARIANT GetDocumentData(const VARIANT& sName);
	void CloseAllForms();
	LPDISPATCH OnLineLinks();
	void SetHeteroscedasticity(const VARIANT& sVarName, const VARIANT& newValue);
	VARIANT GetHeteroscedasticity(const VARIANT& sVarName);
	void FlowsheetCenterView(LPCTSTR pItemID, long itemtype, double scalefactor, long ifactortype);
	void RemoveEstimateVariable(const VARIANT& cvVariable);
	void RemoveAllEstimateVariables();
	void LaunchEstimationView(const VARIANT& Tab);
	void CloseMessagesWindow();
	void CloseExplorerWindows();
	void ShowMessagesWindow(const VARIANT& cvx, const VARIANT& cvy, const VARIANT& cvcx, const VARIANT& cvcy);
	void LaunchExplorer(const VARIANT& cvx, const VARIANT& cvy, const VARIANT& cvcx, const VARIANT& cvcy);
	void ShowFlowsheetWindow(const VARIANT& cvx, const VARIANT& cvy, const VARIANT& cvcx, const VARIANT& cvcy);
	void ShowFlowsheetViewport(const VARIANT& cvleft, const VARIANT& cvright, const VARIANT& cvtop, const VARIANT& cvbottom);
	void CompileType(const VARIANT& cvTypeName);
	void CreateModel(const VARIANT& cvTypeName, const VARIANT& cvTypeText);
	void SetModelText(const VARIANT& cvTypeName, const VARIANT& cvTypeText);
	VARIANT GetModelText(const VARIANT& cvModelName);
	VARIANT GetCurrentFlowsheetContext();
	void CreateType(const VARIANT& cvTypeName, const VARIANT& cvTypeText, const VARIANT& cvFolder);
	void CreateFolder(const VARIANT& cvFolderPath);
	void RemoveFolder(const VARIANT& cvFolderPath);
	void RemoveType(const VARIANT& cvTypeName);
	void SetTypeText(const VARIANT& cvTypeName, const VARIANT& cvTypeText);
	VARIANT GetTypeText(const VARIANT& cvTypeName);
	VARIANT CreateStructure(const VARIANT& cvStructureType, const VARIANT& cvStructureName);
	void CompileModel(const VARIANT& cvModelName);
	void RemoveModel(const VARIANT& cvModelName);
	VARIANT GetEstimationPredictedValues(const VARIANT& cvExperimentName, const VARIANT& cvVariableName);
	void SetEstMeasureVarActive(const VARIANT& sVarName, const VARIANT& newValue);
	VARIANT GetEstMeasureVarActive(const VARIANT& sVarName);
	void CloseFormView(const VARIANT& cvFormPath);
	void ResetAll();
	VARIANT GetPath();
	void RaiseUserEvent(const VARIANT& cvUserString);
	void OpenLibrary(const VARIANT& cvLibraryPath);
	void ImportFlowsheet(const VARIANT& cvImportFilePath);
	void RefreshPlots();
	void CreateLibraryEx(const VARIANT& cvLibraryName, const VARIANT& cvDebug);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerOutputLogger wrapper class

class IAspenModelerOutputLogger : public COleDispatchDriver
{
public:
	IAspenModelerOutputLogger() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerOutputLogger(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerOutputLogger(const IAspenModelerOutputLogger& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetFileOutput();
	void SetFileOutput(const VARIANT&);
	VARIANT GetFullName();
	void SetFullName(const VARIANT&);
	VARIANT GetHeight();
	void SetHeight(const VARIANT&);
	VARIANT GetLeft();
	void SetLeft(const VARIANT&);
	VARIANT GetName();
	void SetName(const VARIANT&);
	VARIANT GetParent();
	void SetParent(const VARIANT&);
	VARIANT GetPath();
	void SetPath(const VARIANT&);
	VARIANT GetScreenOutput();
	void SetScreenOutput(const VARIANT&);
	VARIANT GetTop();
	void SetTop(const VARIANT&);
	VARIANT GetWidth();
	void SetWidth(const VARIANT&);
	VARIANT GetWriteFileHeader();
	void SetWriteFileHeader(const VARIANT&);
	VARIANT GetMessageCount();
	void SetMessageCount(const VARIANT&);
	VARIANT GetMessages();
	void SetMessages(const VARIANT&);

	// Operations
public:
	void ClearWindow();
	void Print();
	VARIANT GetMessageText(const VARIANT& vStartLine, const VARIANT& vLastLine);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerOutputLoggerMessageCollection wrapper class

class IAspenModelerOutputLoggerMessageCollection : public COleDispatchDriver
{
public:
	IAspenModelerOutputLoggerMessageCollection() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerOutputLoggerMessageCollection(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerOutputLoggerMessageCollection(const IAspenModelerOutputLoggerMessageCollection& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetApplication();
	void SetApplication(const VARIANT&);
	VARIANT GetParent();
	void SetParent(const VARIANT&);
	VARIANT GetCount();
	void SetCount(const VARIANT&);

	// Operations
public:
	VARIANT Item(const VARIANT& iSubscript);
	VARIANT _NewEnum();
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEstimationExperiment wrapper class

class IAspenModelerEstimationExperiment : public COleDispatchDriver
{
public:
	IAspenModelerEstimationExperiment() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerEstimationExperiment(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerEstimationExperiment(const IAspenModelerEstimationExperiment& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetWeight();
	void SetWeight(const VARIANT&);

	// Operations
public:
	void AddFixedPoint(const VARIANT& varVariable, const VARIANT& varValue);
	void AddInitialPoint(const VARIANT& varVariable, const VARIANT& varValue);
	void AddRateInitialPoint(const VARIANT& varVariable, const VARIANT& varValue);
	void AddSSMeasurePoint(const VARIANT& varVariable, const VARIANT& varValue, const VARIANT& varWeight);
	void AddDynMeasurePoint(const VARIANT& varVariable, const VARIANT& varTime, const VARIANT& varValue, const VARIANT& varWeight);
	//void SetWeight(const VARIANT& Weight);
	void AddFixedRampPoint(const VARIANT& varVariable, const VARIANT& varTime, const VARIANT& varValue);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEstExperiment wrapper class

class IAspenModelerEstExperiment : public COleDispatchDriver
{
public:
	IAspenModelerEstExperiment() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerEstExperiment(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerEstExperiment(const IAspenModelerEstExperiment& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:

	// Operations
public:
	void AddFixedPoint(const VARIANT& varVariable, const VARIANT& varValue);
	void AddInitialPoint(const VARIANT& varVariable, const VARIANT& varValue);
	void AddRateInitialPoint(const VARIANT& varVariable, const VARIANT& varValue);
	void AddSSMeasurePoint(const VARIANT& varVariable, const VARIANT& varValue, const VARIANT& varWeight);
	void AddDynMeasurePoint(const VARIANT& varVariable, const VARIANT& varValue, const VARIANT& varWeight);
	void SetWeight(const VARIANT& Weight);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerResults wrapper class

class IAspenModelerResults : public COleDispatchDriver
{
public:
	IAspenModelerResults() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerResults(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerResults(const IAspenModelerResults& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetRegularSnapshot();
	void SetRegularSnapshot(const VARIANT&);
	VARIANT GetInterval();
	void SetInterval(const VARIANT&);
	VARIANT GetLimit();
	void SetLimit(const VARIANT&);
	VARIANT GetSnapshotCount();
	void SetSnapshotCount(const VARIANT&);
	VARIANT GetResultCount();
	void SetResultCount(const VARIANT&);

	// Operations
public:
	void TakeSnapshot(const VARIANT& cvName);
	void Refresh();
	VARIANT GetSnapshot(const VARIANT& cvNumber);
	void LoadSnapshot(const VARIANT& cvSnapshot);
	void Delete(const VARIANT& cvSnapshot);
	void CopyValues(const VARIANT& cvSnapshot, const VARIANT& cvFixedSource, const VARIANT& cvFreeSource, const VARIANT& cvInitialSource, const VARIANT& cvFixedDest, const VARIANT& cvFreeDest, const VARIANT& cvInitialDest, 
		const VARIANT& cvSourceString, const VARIANT& cvDestString);
	VARIANT GetResult(const VARIANT& number);
	VARIANT FindResult(const VARIANT& Description);
	VARIANT FindSnapshot(const VARIANT& Description);
	void Import(const VARIANT& ImportFile);
	void CompressFile();
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerSnapshot wrapper class

class IAspenModelerSnapshot : public COleDispatchDriver
{
public:
	IAspenModelerSnapshot() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerSnapshot(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerSnapshot(const IAspenModelerSnapshot& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetDescription();
	void SetDescription(const VARIANT&);
	VARIANT GetSimulationTime();
	void SetSimulationTime(const VARIANT&);
	VARIANT GetRunNumber();
	void SetRunNumber(const VARIANT&);
	VARIANT GetConverged();
	void SetConverged(const VARIANT&);
	VARIANT GetDateTime();
	void SetDateTime(const VARIANT&);

	// Operations
public:
	void ExportasBinary(const VARIANT& ExportFile);
	void Export(const VARIANT& ExportFile);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerUOM wrapper class

class IAspenModelerUOM : public COleDispatchDriver
{
public:
	IAspenModelerUOM() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerUOM(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerUOM(const IAspenModelerUOM& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:

	// Operations
public:
	VARIANT ConvertToBaseUOM(const VARIANT& BaseUOMName, const VARIANT& InputValue);
	VARIANT ConvertFromBaseUOM(const VARIANT& BaseUOMName, const VARIANT& InputValue);
	VARIANT AddConversion(const VARIANT& BaseUOMName, const VARIANT& UOMSystem, const VARIANT& DisplayUOMName, const VARIANT& Multiplier, const VARIANT& Offset);
	VARIANT AddUOMSet(const VARIANT& UOMSystemName);
	VARIANT SelectUOMSet(const VARIANT& UOMSystemName);
	VARIANT AddPhysicalQuantity(const VARIANT& PhysQuanName, const VARIANT& BaseUOMSymbol, const VARIANT& AspenPlusRow, const VARIANT& AspenPlusCol);
	VARIANT DefineConversion(const VARIANT& PhysQuanName, const VARIANT& UOMSymbol, const VARIANT& Factor, const VARIANT& Offset);
	VARIANT DefaultDisplayUOM(const VARIANT& UOMSet, const VARIANT& PhysQuanName, const VARIANT& UOMSymbol);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerOptimization wrapper class

class IAspenModelerOptimization : public COleDispatchDriver
{
public:
	IAspenModelerOptimization() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerOptimization(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerOptimization(const IAspenModelerOptimization& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:
	VARIANT GetEndTime();
	void SetEndTime(const VARIANT&);
	VARIANT GetControlStepInterval();
	void SetControlStepInterval(const VARIANT&);
	VARIANT GetIsDynamic();
	void SetIsDynamic(const VARIANT&);
	VARIANT GetNumberOfElements();
	void SetNumberOfElements(const VARIANT&);
	VARIANT GetEndTimeFixed();
	void SetEndTimeFixed(const VARIANT&);
	VARIANT GetPiecewiseLinear();
	void SetPiecewiseLinear(const VARIANT&);
	VARIANT GetMovingElementSizes();
	void SetMovingElementSizes(const VARIANT&);
	VARIANT GetOptimiseFinalTime();
	void SetOptimiseFinalTime(const VARIANT&);
	VARIANT GetUpperTimeBound();
	void SetUpperTimeBound(const VARIANT&);
	VARIANT GetLowerTimeBound();
	void SetLowerTimeBound(const VARIANT&);
	VARIANT GetUpperElementBound();
	void SetUpperElementBound(const VARIANT&);
	VARIANT GetLowerElementBound();
	void SetLowerElementBound(const VARIANT&);
	VARIANT GetEqualElementSizes();
	void SetEqualElementSizes(const VARIANT&);

	// Operations
public:
	void AddVariable(const VARIANT& VariableName, const VARIANT& vSpecification);
	void ClearVariables();
	void SetControlValue(const VARIANT& VariableName, const VARIANT& Time, const VARIANT& vValue);
	void SetControlElementValue(const VARIANT& VariableName, const VARIANT& ElementNumber, const VARIANT& vValue);
	void SetControlElementTime(const VARIANT& ElementNumber, double dTime);
};
/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEstimation wrapper class

class IAspenModelerEstimation : public COleDispatchDriver
{
public:
	IAspenModelerEstimation() {}		// Calls COleDispatchDriver default constructor
	IAspenModelerEstimation(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	IAspenModelerEstimation(const IAspenModelerEstimation& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

	// Attributes
public:

	// Operations
public:
};

#endif