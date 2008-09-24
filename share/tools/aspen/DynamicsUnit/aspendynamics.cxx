// Machine generated IDispatch wrapper class(es) created with ClassWizard

#include "stdafx.h"
#include "aspendynamics.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



/////////////////////////////////////////////////////////////////////////////
// IAspenModelerPhysicalProperties properties

VARIANT IAspenModelerPhysicalProperties::GetComponentListNames()
{
	VARIANT result;
	GetProperty(0x5101, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerPhysicalProperties::SetComponentListNames(const VARIANT& propVal)
{
	SetProperty(0x5101, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerPhysicalProperties::GetPropertiesFileName()
{
	VARIANT result;
	GetProperty(0x5106, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerPhysicalProperties::SetPropertiesFileName(const VARIANT& propVal)
{
	SetProperty(0x5106, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerPhysicalProperties operations

LPDISPATCH IAspenModelerPhysicalProperties::ComponentList(const VARIANT& sName)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x5102, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		&sName);
	return result;
}

void IAspenModelerPhysicalProperties::AddComponentList(const VARIANT& sName, const VARIANT& sSet)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x5103, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sName, &sSet);
}

void IAspenModelerPhysicalProperties::RemoveComponentList(const VARIANT& sName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x5104, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sName);
}

void IAspenModelerPhysicalProperties::LoadPropertiesFile(const VARIANT& sPropertiesFilename)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x5105, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sPropertiesFilename);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerComponentList properties

VARIANT IAspenModelerComponentList::GetComponents()
{
	VARIANT result;
	GetProperty(0x6101, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerComponentList::SetComponents(const VARIANT& propVal)
{
	SetProperty(0x6101, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerComponentList::GetOptionNames()
{
	VARIANT result;
	GetProperty(0x6102, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerComponentList::SetOptionNames(const VARIANT& propVal)
{
	SetProperty(0x6102, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerComponentList::GetOption()
{
	VARIANT result;
	GetProperty(0x6103, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerComponentList::SetOption(const VARIANT& propVal)
{
	SetProperty(0x6103, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerComponentList::GetName()
{
	VARIANT result;
	GetProperty(0x6104, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerComponentList::SetName(const VARIANT& propVal)
{
	SetProperty(0x6104, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerComponentList::GetIsComponentSet()
{
	VARIANT result;
	GetProperty(0x6105, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerComponentList::SetIsComponentSet(const VARIANT& propVal)
{
	SetProperty(0x6105, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerComponentList operations


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerHomotopy properties

VARIANT IAspenModelerHomotopy::GetCount()
{
	VARIANT result;
	GetProperty(0x4004, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerHomotopy::SetCount(const VARIANT& propVal)
{
	SetProperty(0x4004, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerHomotopy::GetHomotopyEnabled()
{
	VARIANT result;
	GetProperty(0x4009, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerHomotopy::SetHomotopyEnabled(const VARIANT& propVal)
{
	SetProperty(0x4009, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerHomotopy::GetTheta()
{
	VARIANT result;
	GetProperty(0x4010, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerHomotopy::SetTheta(const VARIANT& propVal)
{
	SetProperty(0x4010, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerHomotopy::GetVariable()
{
	VARIANT result;
	GetProperty(0x4011, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerHomotopy::SetVariable(const VARIANT& propVal)
{
	SetProperty(0x4011, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerHomotopy operations

void IAspenModelerHomotopy::AddTarget(const VARIANT& sVarName, const VARIANT& sVarValue, const VARIANT& sVarUnits)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x4002, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sVarName, &sVarValue, &sVarUnits);
}

VARIANT IAspenModelerHomotopy::GetTarget(const VARIANT& sVarName, const VARIANT& sUnit)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x4006, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&sVarName, &sUnit);
	return result;
}

void IAspenModelerHomotopy::RemoveAll()
{
	InvokeHelper(0x4008, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerHomotopy::SetTarget(const VARIANT& sVarName, const VARIANT& sTarget, const VARIANT& sUnit)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x4003, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sVarName, &sTarget, &sUnit);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModeler properties

VARIANT IAspenModeler::GetApplication()
{
	VARIANT result;
	GetProperty(0x0, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetApplication(const VARIANT& propVal)
{
	SetProperty(0x0, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetAppName()
{
	VARIANT result;
	GetProperty(0x10000, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetAppName(const VARIANT& propVal)
{
	SetProperty(0x10000, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetOutputLogger()
{
	VARIANT result;
	GetProperty(0x10001, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetOutputLogger(const VARIANT& propVal)
{
	SetProperty(0x10001, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetFullName()
{
	VARIANT result;
	GetProperty(0x10002, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetFullName(const VARIANT& propVal)
{
	SetProperty(0x10002, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetName()
{
	VARIANT result;
	GetProperty(0x10003, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetName(const VARIANT& propVal)
{
	SetProperty(0x10003, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetVersion()
{
	VARIANT result;
	GetProperty(0x10004, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetVersion(const VARIANT& propVal)
{
	SetProperty(0x10004, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetVisible()
{
	VARIANT result;
	GetProperty(0x10005, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetVisible(const VARIANT& propVal)
{
	SetProperty(0x10005, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetPath()
{
	VARIANT result;
	GetProperty(0x10006, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetPath(const VARIANT& propVal)
{
	SetProperty(0x10006, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetStatusBar()
{
	VARIANT result;
	GetProperty(0x10007, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetStatusBar(const VARIANT& propVal)
{
	SetProperty(0x10007, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetCaption()
{
	VARIANT result;
	GetProperty(0x10008, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetCaption(const VARIANT& propVal)
{
	SetProperty(0x10008, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetDefaultFilePath()
{
	VARIANT result;
	GetProperty(0x10009, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetDefaultFilePath(const VARIANT& propVal)
{
	SetProperty(0x10009, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetInteractive()
{
	VARIANT result;
	GetProperty(0x1000a, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetInteractive(const VARIANT& propVal)
{
	SetProperty(0x1000a, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetSimulation()
{
	VARIANT result;
	GetProperty(0x1000b, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetSimulation(const VARIANT& propVal)
{
	SetProperty(0x1000b, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetHeight()
{
	VARIANT result;
	GetProperty(0x1000c, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetHeight(const VARIANT& propVal)
{
	SetProperty(0x1000c, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetLeft()
{
	VARIANT result;
	GetProperty(0x1000d, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetLeft(const VARIANT& propVal)
{
	SetProperty(0x1000d, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetTop()
{
	VARIANT result;
	GetProperty(0x1000e, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetTop(const VARIANT& propVal)
{
	SetProperty(0x1000e, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetWidth()
{
	VARIANT result;
	GetProperty(0x1000f, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetWidth(const VARIANT& propVal)
{
	SetProperty(0x1000f, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetWindow()
{
	VARIANT result;
	GetProperty(0x10010, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetWindow(const VARIANT& propVal)
{
	SetProperty(0x10010, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetProcessId()
{
	VARIANT result;
	GetProperty(0x10011, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetProcessId(const VARIANT& propVal)
{
	SetProperty(0x10011, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetParent()
{
	VARIANT result;
	GetProperty(0x10012, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetParent(const VARIANT& propVal)
{
	SetProperty(0x10012, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetConfiguration()
{
	VARIANT result;
	GetProperty(0x10013, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetConfiguration(const VARIANT& propVal)
{
	SetProperty(0x10013, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetActiveDocument()
{
	VARIANT result;
	GetProperty(0x10014, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetActiveDocument(const VARIANT& propVal)
{
	SetProperty(0x10014, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetWorkingFolder()
{
	VARIANT result;
	GetProperty(0x10015, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetWorkingFolder(const VARIANT& propVal)
{
	SetProperty(0x10015, VT_VARIANT, &propVal);
}

VARIANT IAspenModeler::GetProcessIDs()
{
	VARIANT result;
	GetProperty(0x10016, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModeler::SetProcessIDs(const VARIANT& propVal)
{
	SetProperty(0x10016, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModeler operations

VARIANT IAspenModeler::DefaultColor()
{
	VARIANT result;
	InvokeHelper(0x800, DISPATCH_METHOD, VT_VARIANT, (void*)&result, NULL);
	return result;
}

void IAspenModeler::PrintToMessageWindow(const VARIANT& Message)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10800, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&Message);
}

void IAspenModeler::Quit()
{
	InvokeHelper(0x10801, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModeler::Msg(const VARIANT& Message)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10802, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&Message);
}

VARIANT IAspenModeler::NewDocument()
{
	VARIANT result;
	InvokeHelper(0x10803, DISPATCH_METHOD, VT_VARIANT, (void*)&result, NULL);
	return result;
}

VARIANT IAspenModeler::OpenDocument(const VARIANT& vDocumentName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10804, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&vDocumentName);
	return result;
}

VARIANT IAspenModeler::CloseDocument(const VARIANT& saveChanges)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10805, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&saveChanges);
	return result;
}

void IAspenModeler::SaveDocument()
{
	InvokeHelper(0x10806, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModeler::SaveDocumentAs(const VARIANT& vPathName, const VARIANT& vReplace)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10807, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&vPathName, &vReplace);
}

void IAspenModeler::Minimize()
{
	InvokeHelper(0x10808, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModeler::Maximize()
{
	InvokeHelper(0x10809, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModeler::Restore()
{
	InvokeHelper(0x1080a, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModeler::Activate()
{
	InvokeHelper(0x1080b, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModeler::ClearApplicationData()
{
	InvokeHelper(0x1080d, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

VARIANT IAspenModeler::AddEventSink(const VARIANT& pSink)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1080e, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&pSink);
	return result;
}

void IAspenModeler::RemoveEventSink(const VARIANT& pSink)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1080f, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&pSink);
}

VARIANT IAspenModeler::GetApplicationData(const VARIANT& sName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10810, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		&sName);
	return result;
}

void IAspenModeler::SetApplicationData(const VARIANT& sName, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10810, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		&sName, &newValue);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEvents properties

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEvents operations

void IAspenModelerEvents::OnRunStarted()
{
	InvokeHelper(0x800, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerEvents::OnRunPaused()
{
	InvokeHelper(0x801, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerEvents::OnHasQuit()
{
	InvokeHelper(0x802, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerEvents::OnHasSaved()
{
	InvokeHelper(0x803, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerEvents::OnSavedAs(const VARIANT& sPath)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x804, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sPath);
}

void IAspenModelerEvents::OnOpened(const VARIANT& sPath)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x805, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sPath);
}

void IAspenModelerEvents::OnNew()
{
	InvokeHelper(0x806, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerEvents::OnRunModeChanged(const VARIANT& sRunMode)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x807, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sRunMode);
}

void IAspenModelerEvents::OnNewBlock(const VARIANT& sBlockName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x808, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sBlockName);
}

void IAspenModelerEvents::OnNewStream(const VARIANT& sStreamName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x809, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sStreamName);
}

void IAspenModelerEvents::OnDeletedBlock(const VARIANT& sBlockName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x80a, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sBlockName);
}

void IAspenModelerEvents::OnDeletedStream(const VARIANT& sStreamName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x80b, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sStreamName);
}

void IAspenModelerEvents::OnUomSetChanged(const VARIANT& sUomSetName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x80c, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sUomSetName);
}

void IAspenModelerEvents::OnStreamConnected(const VARIANT& sStreamName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x80d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sStreamName);
}

void IAspenModelerEvents::OnUserChangedVariable(const VARIANT& sVariableName, const VARIANT& sAttributeName)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x80e, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sVariableName, &sAttributeName);
}

void IAspenModelerEvents::OnStreamDisconnected(const VARIANT& sStreamName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x80f, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sStreamName);
}

void IAspenModelerEvents::OnStepComplete()
{
	InvokeHelper(0x810, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerEvents::OnRewindorCopyValues()
{
	InvokeHelper(0x811, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerEvents::OnUserEvent(const VARIANT& sUserString)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x812, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sUserString);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerDocument properties

VARIANT IAspenModelerDocument::GetProblem()
{
	VARIANT result;
	GetProperty(0x0, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetProblem(const VARIANT& propVal)
{
	SetProperty(0x0, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetFlowsheet()
{
	VARIANT result;
	GetProperty(0x1, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetFlowsheet(const VARIANT& propVal)
{
	SetProperty(0x1, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetApplication()
{
	VARIANT result;
	GetProperty(0x2, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetApplication(const VARIANT& propVal)
{
	SetProperty(0x2, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetOutputLogger()
{
	VARIANT result;
	GetProperty(0x10000, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetOutputLogger(const VARIANT& propVal)
{
	SetProperty(0x10000, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetOptions()
{
	VARIANT result;
	GetProperty(0x10001, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetOptions(const VARIANT& propVal)
{
	SetProperty(0x10001, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetFullName()
{
	VARIANT result;
	GetProperty(0x10002, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetFullName(const VARIANT& propVal)
{
	SetProperty(0x10002, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetName()
{
	VARIANT result;
	GetProperty(0x10003, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetName(const VARIANT& propVal)
{
	SetProperty(0x10003, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetParent()
{
	VARIANT result;
	GetProperty(0x10004, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetParent(const VARIANT& propVal)
{
	SetProperty(0x10004, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetSaved()
{
	VARIANT result;
	GetProperty(0x10005, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetSaved(const VARIANT& propVal)
{
	SetProperty(0x10005, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetDegrees()
{
	VARIANT result;
	GetProperty(0x10006, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetDegrees(const VARIANT& propVal)
{
	SetProperty(0x10006, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetEquations()
{
	VARIANT result;
	GetProperty(0x10007, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetEquations(const VARIANT& propVal)
{
	SetProperty(0x10007, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetRunMode()
{
	VARIANT result;
	GetProperty(0x10008, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetRunMode(const VARIANT& propVal)
{
	SetProperty(0x10008, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetRunNumber()
{
	VARIANT result;
	GetProperty(0x10009, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetRunNumber(const VARIANT& propVal)
{
	SetProperty(0x10009, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetSpecState()
{
	VARIANT result;
	GetProperty(0x1000a, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetSpecState(const VARIANT& propVal)
{
	SetProperty(0x1000a, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetState()
{
	VARIANT result;
	GetProperty(0x1000b, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetState(const VARIANT& propVal)
{
	SetProperty(0x1000b, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetTermination()
{
	VARIANT result;
	GetProperty(0x1000c, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetTermination(const VARIANT& propVal)
{
	SetProperty(0x1000c, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetTime()
{
	VARIANT result;
	GetProperty(0x1000d, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetTime(const VARIANT& propVal)
{
	SetProperty(0x1000d, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetVariables()
{
	VARIANT result;
	GetProperty(0x1000e, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetVariables(const VARIANT& propVal)
{
	SetProperty(0x1000e, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetResults()
{
	VARIANT result;
	GetProperty(0x1000f, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetResults(const VARIANT& propVal)
{
	SetProperty(0x1000f, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetUom()
{
	VARIANT result;
	GetProperty(0x10012, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetUom(const VARIANT& propVal)
{
	SetProperty(0x10012, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetEndTime()
{
	VARIANT result;
	GetProperty(0x10013, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetEndTime(const VARIANT& propVal)
{
	SetProperty(0x10013, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetLeastSquaresObjective()
{
	VARIANT result;
	GetProperty(0x10014, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetLeastSquaresObjective(const VARIANT& propVal)
{
	SetProperty(0x10014, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetCovarianceMatrix()
{
	VARIANT result;
	GetProperty(0x10015, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetCovarianceMatrix(const VARIANT& propVal)
{
	SetProperty(0x10015, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetCorrelationMatrix()
{
	VARIANT result;
	GetProperty(0x10016, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetCorrelationMatrix(const VARIANT& propVal)
{
	SetProperty(0x10016, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetCorrelationMatrixPresent()
{
	VARIANT result;
	GetProperty(0x10017, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetCorrelationMatrixPresent(const VARIANT& propVal)
{
	SetProperty(0x10017, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetDeviationArrayPresent()
{
	VARIANT result;
	GetProperty(0x10019, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetDeviationArrayPresent(const VARIANT& propVal)
{
	SetProperty(0x10019, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetCommunicationInterval()
{
	VARIANT result;
	GetProperty(0x1001a, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetCommunicationInterval(const VARIANT& propVal)
{
	SetProperty(0x1001a, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetSuccessful()
{
	VARIANT result;
	GetProperty(0x1001b, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetSuccessful(const VARIANT& propVal)
{
	SetProperty(0x1001b, VT_VARIANT, &propVal);
}

long IAspenModelerDocument::GetFlowsheetModified()
{
	long result;
	GetProperty(0x1001e, VT_I4, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetFlowsheetModified(long propVal)
{
	SetProperty(0x1001e, VT_I4, propVal);
}

VARIANT IAspenModelerDocument::GetIsModified()
{
	VARIANT result;
	GetProperty(0x1001f, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetIsModified(const VARIANT& propVal)
{
	SetProperty(0x1001f, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetScriptIsRunning()
{
	VARIANT result;
	GetProperty(0x10020, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetScriptIsRunning(const VARIANT& propVal)
{
	SetProperty(0x10020, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetHomotopy()
{
	VARIANT result;
	GetProperty(0x10021, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetHomotopy(const VARIANT& propVal)
{
	SetProperty(0x10021, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetFixedHeteroscedasticity()
{
	VARIANT result;
	GetProperty(0x10022, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetFixedHeteroscedasticity(const VARIANT& propVal)
{
	SetProperty(0x10022, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetEstimationRunState()
{
	VARIANT result;
	GetProperty(0x10023, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetEstimationRunState(const VARIANT& propVal)
{
	SetProperty(0x10023, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetEndStepCount()
{
	VARIANT result;
	GetProperty(0x10024, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetEndStepCount(const VARIANT& propVal)
{
	SetProperty(0x10024, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetEstimatedVariables()
{
	VARIANT result;
	GetProperty(0x10025, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetEstimatedVariables(const VARIANT& propVal)
{
	SetProperty(0x10025, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetDefaultTimeUnit()
{
	VARIANT result;
	GetProperty(0x10026, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetDefaultTimeUnit(const VARIANT& propVal)
{
	SetProperty(0x10026, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetExperimentTimeUnit()
{
	VARIANT result;
	GetProperty(0x10027, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetExperimentTimeUnit(const VARIANT& propVal)
{
	SetProperty(0x10027, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetDisplayTime()
{
	VARIANT result;
	GetProperty(0x10028, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetDisplayTime(const VARIANT& propVal)
{
	SetProperty(0x10028, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetDisplayTimeUnit()
{
	VARIANT result;
	GetProperty(0x1002a, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetDisplayTimeUnit(const VARIANT& propVal)
{
	SetProperty(0x1002a, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetFlowsheetWallpaperMode()
{
	VARIANT result;
	GetProperty(0x10835, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetFlowsheetWallpaperMode(const VARIANT& propVal)
{
	SetProperty(0x10835, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetLastError()
{
	VARIANT result;
	GetProperty(0x10855, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetLastError(const VARIANT& propVal)
{
	SetProperty(0x10855, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerDocument::GetGetEstimationMatrixVariables()
{
	VARIANT result;
	GetProperty(0x1085e, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerDocument::SetGetEstimationMatrixVariables(const VARIANT& propVal)
{
	SetProperty(0x1085e, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerDocument operations

void IAspenModelerDocument::Copy(const VARIANT& sPath)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x800, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sPath);
}

void IAspenModelerDocument::PasteLink(const VARIANT& sPath)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x801, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&sPath);
}

void IAspenModelerDocument::ShowWindow(long bShow)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x10800, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		bShow);
}

long IAspenModelerDocument::SetSolverProperty(LPCTSTR sSolverOptionName, const VARIANT& vSolverOptionValue)
{
	long result;
	static BYTE parms[] =
		VTS_BSTR VTS_VARIANT;
	InvokeHelper(0x10801, DISPATCH_METHOD, VT_I4, (void*)&result, parms,
		sSolverOptionName, &vSolverOptionValue);
	return result;
}

long IAspenModelerDocument::GetSolverProperty(LPCTSTR sSolverOptionName, VARIANT* pSolverOptionValue)
{
	long result;
	static BYTE parms[] =
		VTS_BSTR VTS_PVARIANT;
	InvokeHelper(0x10802, DISPATCH_METHOD, VT_I4, (void*)&result, parms,
		sSolverOptionName, pSolverOptionValue);
	return result;
}

void IAspenModelerDocument::Close(const VARIANT& saveChanges, const VARIANT& fileName)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10803, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&saveChanges, &fileName);
}

void IAspenModelerDocument::LaunchFormView(const VARIANT& cvPlotPath, const VARIANT& cvx, const VARIANT& cvy, const VARIANT& cvcx, const VARIANT& cvcy, const VARIANT& cbHistory)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10805, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvPlotPath, &cvx, &cvy, &cvcx, &cvcy, &cbHistory);
}

VARIANT IAspenModelerDocument::NewExperiment(const VARIANT& cvName, const VARIANT& cvRunMode)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10808, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvName, &cvRunMode);
	return result;
}

void IAspenModelerDocument::AddExperiment(const VARIANT& cvExperiment)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10809, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvExperiment);
}

void IAspenModelerDocument::AddEstimateVariable(const VARIANT& cvVariable)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1080a, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvVariable);
}

VARIANT IAspenModelerDocument::GetEstimatedVariableValue(const VARIANT& varVariable)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1080b, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&varVariable);
	return result;
}

VARIANT IAspenModelerDocument::GetEstimatedVariableDeviation(const VARIANT& varName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1080c, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&varName);
	return result;
}

void IAspenModelerDocument::ResetEstimation()
{
	InvokeHelper(0x1080d, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerDocument::Run(const VARIANT& cvWait)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1080e, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvWait);
}

void IAspenModelerDocument::Step(const VARIANT& cvWait)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1080f, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvWait);
}

void IAspenModelerDocument::Pause()
{
	InvokeHelper(0x10810, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

VARIANT IAspenModelerDocument::GetStatusMessage(const VARIANT& cvStatus)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10811, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvStatus);
	return result;
}

void IAspenModelerDocument::CreateLibrary(const VARIANT& cvLibraryName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10812, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvLibraryName);
}

void IAspenModelerDocument::AddSensitivityParameter(const VARIANT& cvVarName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10813, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvVarName);
}

void IAspenModelerDocument::AddSensitivityVariable(const VARIANT& cvVarName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10814, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvVarName);
}

void IAspenModelerDocument::ClearSensitivities()
{
	InvokeHelper(0x10815, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerDocument::EnableSensitivities()
{
	InvokeHelper(0x10816, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerDocument::DisableSensitivities()
{
	InvokeHelper(0x10817, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

VARIANT IAspenModelerDocument::GetSensitivityValue(const VARIANT& cvVarName, const VARIANT& cvParmName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10818, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvVarName, &cvParmName);
	return result;
}

void IAspenModelerDocument::Reset()
{
	InvokeHelper(0x10819, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerDocument::Restart()
{
	InvokeHelper(0x1081a, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

LPDISPATCH IAspenModelerDocument::Optimization()
{
	LPDISPATCH result;
	InvokeHelper(0x1081b, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

void IAspenModelerDocument::SaveDocument()
{
	InvokeHelper(0x1081d, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerDocument::SaveDocumentAs(const VARIANT& vPathName, const VARIANT& vReplace)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x1081e, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&vPathName, &vReplace);
}

LPDISPATCH IAspenModelerDocument::CDI()
{
	LPDISPATCH result;
	InvokeHelper(0x1081f, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

LPDISPATCH IAspenModelerDocument::Properties()
{
	LPDISPATCH result;
	InvokeHelper(0x10820, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

void IAspenModelerDocument::Interrupt(const VARIANT& bWait)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10821, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&bWait);
}

void IAspenModelerDocument::LaunchAppView(const VARIANT& cvGUID)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10822, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvGUID);
}

void IAspenModelerDocument::ClearDocumentData()
{
	InvokeHelper(0x10823, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

VARIANT IAspenModelerDocument::GetCorrelation(const VARIANT& cvEstVar1, const VARIANT& cvEstVar2)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10824, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvEstVar1, &cvEstVar2);
	return result;
}

VARIANT IAspenModelerDocument::GetCovariance(const VARIANT& cvEstVar1, const VARIANT& cvEstVar2)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10825, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvEstVar1, &cvEstVar2);
	return result;
}

VARIANT IAspenModelerDocument::GetDeviation(const VARIANT& cvEstVar)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10826, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvEstVar);
	return result;
}

VARIANT IAspenModelerDocument::GetEstimatedValue(const VARIANT& cvEstVar)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10827, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvEstVar);
	return result;
}

void IAspenModelerDocument::SetDocumentData(const VARIANT& sName, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10828, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		&sName, &newValue);
}

VARIANT IAspenModelerDocument::GetDocumentData(const VARIANT& sName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10828, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		&sName);
	return result;
}

void IAspenModelerDocument::CloseAllForms()
{
	InvokeHelper(0x10829, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

LPDISPATCH IAspenModelerDocument::OnLineLinks()
{
	LPDISPATCH result;
	InvokeHelper(0x1082a, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

void IAspenModelerDocument::SetHeteroscedasticity(const VARIANT& sVarName, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x1082b, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		&sVarName, &newValue);
}

VARIANT IAspenModelerDocument::GetHeteroscedasticity(const VARIANT& sVarName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1082b, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		&sVarName);
	return result;
}

void IAspenModelerDocument::FlowsheetCenterView(LPCTSTR pItemID, long itemtype, double scalefactor, long ifactortype)
{
	static BYTE parms[] =
		VTS_BSTR VTS_I4 VTS_R8 VTS_I4;
	InvokeHelper(0x1082c, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		pItemID, itemtype, scalefactor, ifactortype);
}

void IAspenModelerDocument::RemoveEstimateVariable(const VARIANT& cvVariable)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1082d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvVariable);
}

void IAspenModelerDocument::RemoveAllEstimateVariables()
{
	InvokeHelper(0x1082e, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerDocument::LaunchEstimationView(const VARIANT& Tab)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1082f, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&Tab);
}

void IAspenModelerDocument::CloseMessagesWindow()
{
	InvokeHelper(0x10830, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerDocument::CloseExplorerWindows()
{
	InvokeHelper(0x10831, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerDocument::ShowMessagesWindow(const VARIANT& cvx, const VARIANT& cvy, const VARIANT& cvcx, const VARIANT& cvcy)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10832, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvx, &cvy, &cvcx, &cvcy);
}

void IAspenModelerDocument::LaunchExplorer(const VARIANT& cvx, const VARIANT& cvy, const VARIANT& cvcx, const VARIANT& cvcy)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10833, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvx, &cvy, &cvcx, &cvcy);
}

void IAspenModelerDocument::ShowFlowsheetWindow(const VARIANT& cvx, const VARIANT& cvy, const VARIANT& cvcx, const VARIANT& cvcy)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10834, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvx, &cvy, &cvcx, &cvcy);
}

void IAspenModelerDocument::ShowFlowsheetViewport(const VARIANT& cvleft, const VARIANT& cvright, const VARIANT& cvtop, const VARIANT& cvbottom)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10836, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvleft, &cvright, &cvtop, &cvbottom);
}

void IAspenModelerDocument::CompileType(const VARIANT& cvTypeName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10837, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvTypeName);
}

void IAspenModelerDocument::CreateModel(const VARIANT& cvTypeName, const VARIANT& cvTypeText)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10838, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvTypeName, &cvTypeText);
}

void IAspenModelerDocument::SetModelText(const VARIANT& cvTypeName, const VARIANT& cvTypeText)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10839, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvTypeName, &cvTypeText);
}

VARIANT IAspenModelerDocument::GetModelText(const VARIANT& cvModelName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10840, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvModelName);
	return result;
}

VARIANT IAspenModelerDocument::GetCurrentFlowsheetContext()
{
	VARIANT result;
	InvokeHelper(0x10841, DISPATCH_METHOD, VT_VARIANT, (void*)&result, NULL);
	return result;
}

void IAspenModelerDocument::CreateType(const VARIANT& cvTypeName, const VARIANT& cvTypeText, const VARIANT& cvFolder)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10843, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvTypeName, &cvTypeText, &cvFolder);
}

void IAspenModelerDocument::CreateFolder(const VARIANT& cvFolderPath)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10844, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvFolderPath);
}

void IAspenModelerDocument::RemoveFolder(const VARIANT& cvFolderPath)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10845, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvFolderPath);
}

void IAspenModelerDocument::RemoveType(const VARIANT& cvTypeName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10846, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvTypeName);
}

void IAspenModelerDocument::SetTypeText(const VARIANT& cvTypeName, const VARIANT& cvTypeText)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10847, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvTypeName, &cvTypeText);
}

VARIANT IAspenModelerDocument::GetTypeText(const VARIANT& cvTypeName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10848, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvTypeName);
	return result;
}

VARIANT IAspenModelerDocument::CreateStructure(const VARIANT& cvStructureType, const VARIANT& cvStructureName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10849, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvStructureType, &cvStructureName);
	return result;
}

void IAspenModelerDocument::CompileModel(const VARIANT& cvModelName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10850, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvModelName);
}

void IAspenModelerDocument::RemoveModel(const VARIANT& cvModelName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10851, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvModelName);
}

VARIANT IAspenModelerDocument::GetEstimationPredictedValues(const VARIANT& cvExperimentName, const VARIANT& cvVariableName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10854, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvExperimentName, &cvVariableName);
	return result;
}

void IAspenModelerDocument::SetEstMeasureVarActive(const VARIANT& sVarName, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10857, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		&sVarName, &newValue);
}

VARIANT IAspenModelerDocument::GetEstMeasureVarActive(const VARIANT& sVarName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10857, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		&sVarName);
	return result;
}

void IAspenModelerDocument::CloseFormView(const VARIANT& cvFormPath)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x10858, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvFormPath);
}

void IAspenModelerDocument::ResetAll()
{
	InvokeHelper(0x10859, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

VARIANT IAspenModelerDocument::GetPath()
{
	VARIANT result;
	InvokeHelper(0x1085a, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, NULL);
	return result;
}

void IAspenModelerDocument::RaiseUserEvent(const VARIANT& cvUserString)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1085b, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvUserString);
}

void IAspenModelerDocument::OpenLibrary(const VARIANT& cvLibraryPath)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1085c, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvLibraryPath);
}

void IAspenModelerDocument::ImportFlowsheet(const VARIANT& cvImportFilePath)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x1085f, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvImportFilePath);
}

void IAspenModelerDocument::RefreshPlots()
{
	InvokeHelper(0x10860, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerDocument::CreateLibraryEx(const VARIANT& cvLibraryName, const VARIANT& cvDebug)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x10861, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvLibraryName, &cvDebug);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerOutputLogger properties

VARIANT IAspenModelerOutputLogger::GetFileOutput()
{
	VARIANT result;
	GetProperty(0x0, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetFileOutput(const VARIANT& propVal)
{
	SetProperty(0x0, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetFullName()
{
	VARIANT result;
	GetProperty(0x1, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetFullName(const VARIANT& propVal)
{
	SetProperty(0x1, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetHeight()
{
	VARIANT result;
	GetProperty(0x2, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetHeight(const VARIANT& propVal)
{
	SetProperty(0x2, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetLeft()
{
	VARIANT result;
	GetProperty(0x3, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetLeft(const VARIANT& propVal)
{
	SetProperty(0x3, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetName()
{
	VARIANT result;
	GetProperty(0x4, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetName(const VARIANT& propVal)
{
	SetProperty(0x4, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetParent()
{
	VARIANT result;
	GetProperty(0x5, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetParent(const VARIANT& propVal)
{
	SetProperty(0x5, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetPath()
{
	VARIANT result;
	GetProperty(0x6, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetPath(const VARIANT& propVal)
{
	SetProperty(0x6, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetScreenOutput()
{
	VARIANT result;
	GetProperty(0x7, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetScreenOutput(const VARIANT& propVal)
{
	SetProperty(0x7, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetTop()
{
	VARIANT result;
	GetProperty(0x8, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetTop(const VARIANT& propVal)
{
	SetProperty(0x8, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetWidth()
{
	VARIANT result;
	GetProperty(0x9, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetWidth(const VARIANT& propVal)
{
	SetProperty(0x9, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetWriteFileHeader()
{
	VARIANT result;
	GetProperty(0xa, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetWriteFileHeader(const VARIANT& propVal)
{
	SetProperty(0xa, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetMessageCount()
{
	VARIANT result;
	GetProperty(0xb, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetMessageCount(const VARIANT& propVal)
{
	SetProperty(0xb, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLogger::GetMessages()
{
	VARIANT result;
	GetProperty(0xc, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLogger::SetMessages(const VARIANT& propVal)
{
	SetProperty(0xc, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerOutputLogger operations

void IAspenModelerOutputLogger::ClearWindow()
{
	InvokeHelper(0x800, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerOutputLogger::Print()
{
	InvokeHelper(0x801, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

VARIANT IAspenModelerOutputLogger::GetMessageText(const VARIANT& vStartLine, const VARIANT& vLastLine)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x802, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		&vStartLine, &vLastLine);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerOutputLoggerMessageCollection properties

VARIANT IAspenModelerOutputLoggerMessageCollection::GetApplication()
{
	VARIANT result;
	GetProperty(0x0, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLoggerMessageCollection::SetApplication(const VARIANT& propVal)
{
	SetProperty(0x0, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLoggerMessageCollection::GetParent()
{
	VARIANT result;
	GetProperty(0x1, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLoggerMessageCollection::SetParent(const VARIANT& propVal)
{
	SetProperty(0x1, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOutputLoggerMessageCollection::GetCount()
{
	VARIANT result;
	GetProperty(0x2, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOutputLoggerMessageCollection::SetCount(const VARIANT& propVal)
{
	SetProperty(0x2, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerOutputLoggerMessageCollection operations

VARIANT IAspenModelerOutputLoggerMessageCollection::Item(const VARIANT& iSubscript)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x800, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&iSubscript);
	return result;
}

VARIANT IAspenModelerOutputLoggerMessageCollection::_NewEnum()
{
	VARIANT result;
	InvokeHelper(0xfffffffc, DISPATCH_METHOD, VT_VARIANT, (void*)&result, NULL);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEstimationExperiment properties

VARIANT IAspenModelerEstimationExperiment::GetWeight()
{
	VARIANT result;
	GetProperty(0x1, VT_VARIANT, (void*)&result);
	return result;
}

//void IAspenModelerEstimationExperiment::SetWeight(const VARIANT& propVal)
//{
//	SetProperty(0x1, VT_VARIANT, &propVal);
//}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEstimationExperiment operations

void IAspenModelerEstimationExperiment::AddFixedPoint(const VARIANT& varVariable, const VARIANT& varValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x800, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varValue);
}

void IAspenModelerEstimationExperiment::AddInitialPoint(const VARIANT& varVariable, const VARIANT& varValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x801, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varValue);
}

void IAspenModelerEstimationExperiment::AddRateInitialPoint(const VARIANT& varVariable, const VARIANT& varValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x802, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varValue);
}

void IAspenModelerEstimationExperiment::AddSSMeasurePoint(const VARIANT& varVariable, const VARIANT& varValue, const VARIANT& varWeight)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x803, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varValue, &varWeight);
}

void IAspenModelerEstimationExperiment::AddDynMeasurePoint(const VARIANT& varVariable, const VARIANT& varTime, const VARIANT& varValue, const VARIANT& varWeight)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x804, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varTime, &varValue, &varWeight);
}

void IAspenModelerEstimationExperiment::SetWeight(const VARIANT& Weight)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x805, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&Weight);
}

void IAspenModelerEstimationExperiment::AddFixedRampPoint(const VARIANT& varVariable, const VARIANT& varTime, const VARIANT& varValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x806, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varTime, &varValue);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEstExperiment properties

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEstExperiment operations

void IAspenModelerEstExperiment::AddFixedPoint(const VARIANT& varVariable, const VARIANT& varValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x800, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varValue);
}

void IAspenModelerEstExperiment::AddInitialPoint(const VARIANT& varVariable, const VARIANT& varValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x801, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varValue);
}

void IAspenModelerEstExperiment::AddRateInitialPoint(const VARIANT& varVariable, const VARIANT& varValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x802, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varValue);
}

void IAspenModelerEstExperiment::AddSSMeasurePoint(const VARIANT& varVariable, const VARIANT& varValue, const VARIANT& varWeight)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x803, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varValue, &varWeight);
}

void IAspenModelerEstExperiment::AddDynMeasurePoint(const VARIANT& varVariable, const VARIANT& varValue, const VARIANT& varWeight)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x804, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&varVariable, &varValue, &varWeight);
}

void IAspenModelerEstExperiment::SetWeight(const VARIANT& Weight)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x805, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&Weight);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerResults properties

VARIANT IAspenModelerResults::GetRegularSnapshot()
{
	VARIANT result;
	GetProperty(0x0, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerResults::SetRegularSnapshot(const VARIANT& propVal)
{
	SetProperty(0x0, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerResults::GetInterval()
{
	VARIANT result;
	GetProperty(0x1, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerResults::SetInterval(const VARIANT& propVal)
{
	SetProperty(0x1, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerResults::GetLimit()
{
	VARIANT result;
	GetProperty(0x2, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerResults::SetLimit(const VARIANT& propVal)
{
	SetProperty(0x2, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerResults::GetSnapshotCount()
{
	VARIANT result;
	GetProperty(0x3, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerResults::SetSnapshotCount(const VARIANT& propVal)
{
	SetProperty(0x3, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerResults::GetResultCount()
{
	VARIANT result;
	GetProperty(0x4, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerResults::SetResultCount(const VARIANT& propVal)
{
	SetProperty(0x4, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerResults operations

void IAspenModelerResults::TakeSnapshot(const VARIANT& cvName)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x800, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvName);
}

void IAspenModelerResults::Refresh()
{
	InvokeHelper(0x801, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

VARIANT IAspenModelerResults::GetSnapshot(const VARIANT& cvNumber)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x802, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&cvNumber);
	return result;
}

void IAspenModelerResults::LoadSnapshot(const VARIANT& cvSnapshot)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x803, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvSnapshot);
}

void IAspenModelerResults::Delete(const VARIANT& cvSnapshot)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x804, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvSnapshot);
}

void IAspenModelerResults::CopyValues(const VARIANT& cvSnapshot, const VARIANT& cvFixedSource, const VARIANT& cvFreeSource, const VARIANT& cvInitialSource, const VARIANT& cvFixedDest, const VARIANT& cvFreeDest, const VARIANT& cvInitialDest, 
									  const VARIANT& cvSourceString, const VARIANT& cvDestString)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x805, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&cvSnapshot, &cvFixedSource, &cvFreeSource, &cvInitialSource, &cvFixedDest, &cvFreeDest, &cvInitialDest, &cvSourceString, &cvDestString);
}

VARIANT IAspenModelerResults::GetResult(const VARIANT& number)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x806, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&number);
	return result;
}

VARIANT IAspenModelerResults::FindResult(const VARIANT& Description)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x807, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&Description);
	return result;
}

VARIANT IAspenModelerResults::FindSnapshot(const VARIANT& Description)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x808, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&Description);
	return result;
}

void IAspenModelerResults::Import(const VARIANT& ImportFile)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x809, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&ImportFile);
}

void IAspenModelerResults::CompressFile()
{
	InvokeHelper(0x810, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerSnapshot properties

VARIANT IAspenModelerSnapshot::GetDescription()
{
	VARIANT result;
	GetProperty(0x0, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerSnapshot::SetDescription(const VARIANT& propVal)
{
	SetProperty(0x0, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerSnapshot::GetSimulationTime()
{
	VARIANT result;
	GetProperty(0x1, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerSnapshot::SetSimulationTime(const VARIANT& propVal)
{
	SetProperty(0x1, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerSnapshot::GetRunNumber()
{
	VARIANT result;
	GetProperty(0x2, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerSnapshot::SetRunNumber(const VARIANT& propVal)
{
	SetProperty(0x2, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerSnapshot::GetConverged()
{
	VARIANT result;
	GetProperty(0x3, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerSnapshot::SetConverged(const VARIANT& propVal)
{
	SetProperty(0x3, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerSnapshot::GetDateTime()
{
	VARIANT result;
	GetProperty(0x7, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerSnapshot::SetDateTime(const VARIANT& propVal)
{
	SetProperty(0x7, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerSnapshot operations

void IAspenModelerSnapshot::ExportasBinary(const VARIANT& ExportFile)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x803, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&ExportFile);
}

void IAspenModelerSnapshot::Export(const VARIANT& ExportFile)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x804, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&ExportFile);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerUOM properties

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerUOM operations

VARIANT IAspenModelerUOM::ConvertToBaseUOM(const VARIANT& BaseUOMName, const VARIANT& InputValue)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x800, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&BaseUOMName, &InputValue);
	return result;
}

VARIANT IAspenModelerUOM::ConvertFromBaseUOM(const VARIANT& BaseUOMName, const VARIANT& InputValue)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x801, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&BaseUOMName, &InputValue);
	return result;
}

VARIANT IAspenModelerUOM::AddConversion(const VARIANT& BaseUOMName, const VARIANT& UOMSystem, const VARIANT& DisplayUOMName, const VARIANT& Multiplier, const VARIANT& Offset)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x802, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&BaseUOMName, &UOMSystem, &DisplayUOMName, &Multiplier, &Offset);
	return result;
}

VARIANT IAspenModelerUOM::AddUOMSet(const VARIANT& UOMSystemName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x803, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&UOMSystemName);
	return result;
}

VARIANT IAspenModelerUOM::SelectUOMSet(const VARIANT& UOMSystemName)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x804, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&UOMSystemName);
	return result;
}

VARIANT IAspenModelerUOM::AddPhysicalQuantity(const VARIANT& PhysQuanName, const VARIANT& BaseUOMSymbol, const VARIANT& AspenPlusRow, const VARIANT& AspenPlusCol)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x805, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&PhysQuanName, &BaseUOMSymbol, &AspenPlusRow, &AspenPlusCol);
	return result;
}

VARIANT IAspenModelerUOM::DefineConversion(const VARIANT& PhysQuanName, const VARIANT& UOMSymbol, const VARIANT& Factor, const VARIANT& Offset)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x806, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&PhysQuanName, &UOMSymbol, &Factor, &Offset);
	return result;
}

VARIANT IAspenModelerUOM::DefaultDisplayUOM(const VARIANT& UOMSet, const VARIANT& PhysQuanName, const VARIANT& UOMSymbol)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x807, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&UOMSet, &PhysQuanName, &UOMSymbol);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerOptimization properties

VARIANT IAspenModelerOptimization::GetEndTime()
{
	VARIANT result;
	GetProperty(0x400a, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetEndTime(const VARIANT& propVal)
{
	SetProperty(0x400a, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetControlStepInterval()
{
	VARIANT result;
	GetProperty(0x4007, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetControlStepInterval(const VARIANT& propVal)
{
	SetProperty(0x4007, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetIsDynamic()
{
	VARIANT result;
	GetProperty(0x4008, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetIsDynamic(const VARIANT& propVal)
{
	SetProperty(0x4008, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetNumberOfElements()
{
	VARIANT result;
	GetProperty(0x400c, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetNumberOfElements(const VARIANT& propVal)
{
	SetProperty(0x400c, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetEndTimeFixed()
{
	VARIANT result;
	GetProperty(0x400f, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetEndTimeFixed(const VARIANT& propVal)
{
	SetProperty(0x400f, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetPiecewiseLinear()
{
	VARIANT result;
	GetProperty(0x4010, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetPiecewiseLinear(const VARIANT& propVal)
{
	SetProperty(0x4010, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetMovingElementSizes()
{
	VARIANT result;
	GetProperty(0x4011, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetMovingElementSizes(const VARIANT& propVal)
{
	SetProperty(0x4011, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetOptimiseFinalTime()
{
	VARIANT result;
	GetProperty(0x4012, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetOptimiseFinalTime(const VARIANT& propVal)
{
	SetProperty(0x4012, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetUpperTimeBound()
{
	VARIANT result;
	GetProperty(0x4013, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetUpperTimeBound(const VARIANT& propVal)
{
	SetProperty(0x4013, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetLowerTimeBound()
{
	VARIANT result;
	GetProperty(0x4014, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetLowerTimeBound(const VARIANT& propVal)
{
	SetProperty(0x4014, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetUpperElementBound()
{
	VARIANT result;
	GetProperty(0x4015, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetUpperElementBound(const VARIANT& propVal)
{
	SetProperty(0x4015, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetLowerElementBound()
{
	VARIANT result;
	GetProperty(0x4016, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetLowerElementBound(const VARIANT& propVal)
{
	SetProperty(0x4016, VT_VARIANT, &propVal);
}

VARIANT IAspenModelerOptimization::GetEqualElementSizes()
{
	VARIANT result;
	GetProperty(0x4017, VT_VARIANT, (void*)&result);
	return result;
}

void IAspenModelerOptimization::SetEqualElementSizes(const VARIANT& propVal)
{
	SetProperty(0x4017, VT_VARIANT, &propVal);
}

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerOptimization operations

void IAspenModelerOptimization::AddVariable(const VARIANT& VariableName, const VARIANT& vSpecification)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x4001, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&VariableName, &vSpecification);
}

void IAspenModelerOptimization::ClearVariables()
{
	InvokeHelper(0x4003, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAspenModelerOptimization::SetControlValue(const VARIANT& VariableName, const VARIANT& Time, const VARIANT& vValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x4009, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&VariableName, &Time, &vValue);
}

void IAspenModelerOptimization::SetControlElementValue(const VARIANT& VariableName, const VARIANT& ElementNumber, const VARIANT& vValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x400d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&VariableName, &ElementNumber, &vValue);
}

void IAspenModelerOptimization::SetControlElementTime(const VARIANT& ElementNumber, double dTime)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_R8;
	InvokeHelper(0x400e, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		&ElementNumber, dTime);
}


/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEstimation properties

/////////////////////////////////////////////////////////////////////////////
// IAspenModelerEstimation operations
