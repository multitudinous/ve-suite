// Machine generated IDispatch wrapper class(es) created with ClassWizard

#include "stdafx.h"
#include "happ.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



/////////////////////////////////////////////////////////////////////////////
// IHNode properties

/////////////////////////////////////////////////////////////////////////////
// IHNode operations

LPDISPATCH IHNode::GetApplication()
{
	LPDISPATCH result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

LPDISPATCH IHNode::GetParent()
{
	LPDISPATCH result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

CString IHNode::GetName(const VARIANT& force)
{
	CString result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		&force);
	return result;
}

void IHNode::SetName(const VARIANT& force, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_BSTR;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 &force, lpszNewValue);
}

long IHNode::GetDimension()
{
	long result;
	InvokeHelper(0x60020004, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

short IHNode::GetValueType()
{
	short result;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

VARIANT IHNode::GetValue(const VARIANT& force)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		&force);
	return result;
}

void IHNode::SetValue(const VARIANT& force, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 &force, &newValue);
}

void IHNode::SetValueAndUnit(const VARIANT& Value, short unitcol, const VARIANT& force)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_I2 VTS_VARIANT;
	InvokeHelper(0x60020008, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &Value, unitcol, &force);
}

void IHNode::SetValueUnitAndBasis(const VARIANT& Value, short unitcol, LPCTSTR basis, const VARIANT& force)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_I2 VTS_BSTR VTS_VARIANT;
	InvokeHelper(0x60020009, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &Value, unitcol, basis, &force);
}

BOOL IHNode::GetHasAttribute(short attrnumber)
{
	BOOL result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x6002000a, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		attrnumber);
	return result;
}

short IHNode::GetAttributeType(short attrnumber)
{
	short result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x6002000b, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, parms,
		attrnumber);
	return result;
}

VARIANT IHNode::GetAttributeValue(short attrnumber, const VARIANT& force)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_I2 VTS_VARIANT;
	InvokeHelper(0x6002000c, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		attrnumber, &force);
	return result;
}

void IHNode::SetAttributeValue(short attrnumber, const VARIANT& force, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x6002000c, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 attrnumber, &force, &newValue);
}

BOOL IHNode::GetHasClassAttribute(LPCTSTR classid)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x6002000e, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		classid);
	return result;
}

short IHNode::GetClassAttributeType(LPCTSTR classid)
{
	short result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x6002000f, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, parms,
		classid);
	return result;
}

VARIANT IHNode::GetClassAttributeValue(LPCTSTR classid, const VARIANT& force)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_BSTR VTS_VARIANT;
	InvokeHelper(0x60020010, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		classid, &force);
	return result;
}

void IHNode::SetClassAttributeValue(LPCTSTR classid, const VARIANT& force, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_BSTR VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020010, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 classid, &force, &newValue);
}

LPDISPATCH IHNode::GetElements()
{
	LPDISPATCH result;
	InvokeHelper(0x60020012, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

void IHNode::Delete()
{
	InvokeHelper(0x60020014, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHNode::RemoveAll()
{
	InvokeHelper(0x60020015, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHNode::AddClassAttribute(LPCTSTR classid, short type)
{
	static BYTE parms[] =
		VTS_BSTR VTS_I2;
	InvokeHelper(0x60020016, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 classid, type);
}

void IHNode::DeleteClassAttribute(LPCTSTR classid)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020017, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 classid);
}

void IHNode::PrintUseful(short append, LPCTSTR filename)
{
	static BYTE parms[] =
		VTS_I2 VTS_BSTR;
	InvokeHelper(0x6002001a, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 append, filename);
}

LPDISPATCH IHNode::FindNode(LPCTSTR path)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x6002001b, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		path);
	return result;
}

CString IHNode::NextIncomplete(VARIANT* code)
{
	CString result;
	static BYTE parms[] =
		VTS_PVARIANT;
	InvokeHelper(0x60020020, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms,
		code);
	return result;
}

CString IHNode::BrowseNext(short direction, short io)
{
	CString result;
	static BYTE parms[] =
		VTS_I2 VTS_I2;
	InvokeHelper(0x60020021, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms,
		direction, io);
	return result;
}

void IHNode::Hide(LPCTSTR Name)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020023, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

void IHNode::Reveal(LPCTSTR Name)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020024, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

void IHNode::Clear()
{
	InvokeHelper(0x60020025, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHNode::NewChild(BSTR* Name)
{
	static BYTE parms[] =
		VTS_PBSTR;
	InvokeHelper(0x60020029, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

void IHNode::RenameChild(BSTR* Name)
{
	static BYTE parms[] =
		VTS_PBSTR;
	InvokeHelper(0x6002002a, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

CString IHNode::GetUnitString()
{
	CString result;
	InvokeHelper(0x6002002d, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

VARIANT IHNode::GetValueForUnit(short unitrow, short unitcol)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_I2 VTS_I2;
	InvokeHelper(0x6002002e, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		unitrow, unitcol);
	return result;
}

void IHNode::Reconcile(long code)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020031, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 code);
}

void IHNode::NewID(BSTR* Name)
{
	static BYTE parms[] =
		VTS_PBSTR;
	InvokeHelper(0x60020032, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

void IHNode::Export(LPCTSTR filename)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020033, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename);
}


/////////////////////////////////////////////////////////////////////////////
// IHapp properties

/////////////////////////////////////////////////////////////////////////////
// IHapp operations

LPDISPATCH IHapp::GetApplication()
{
	LPDISPATCH result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

LPDISPATCH IHapp::GetParent()
{
	LPDISPATCH result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

CString IHapp::GetFullName()
{
	CString result;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

CString IHapp::GetName()
{
	CString result;
	InvokeHelper(0x0, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

BOOL IHapp::GetVisible()
{
	BOOL result;
	InvokeHelper(0x60020004, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHapp::SetVisible(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020004, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

LPDISPATCH IHapp::GetTree()
{
	LPDISPATCH result;
	InvokeHelper(0x6002000b, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

void IHapp::Save()
{
	InvokeHelper(0x6002000c, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHapp::SaveAs(BSTR* filename, const VARIANT& overwrite)
{
	static BYTE parms[] =
		VTS_PBSTR VTS_VARIANT;
	InvokeHelper(0x6002000d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, &overwrite);
}

void IHapp::Close(const VARIANT& reserved)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020010, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &reserved);
}

LPDISPATCH IHapp::GetChoose(short* flag)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_PI2;
	InvokeHelper(0x60020011, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		flag);
	return result;
}

LPDISPATCH IHapp::NewSelection(LPCTSTR Key)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020012, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		Key);
	return result;
}

LPDISPATCH IHapp::GetSelection(LPCTSTR Key)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020013, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		Key);
	return result;
}

void IHapp::DeleteSelection(LPCTSTR Key)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020014, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Key);
}

void IHapp::SaveSelection(LPCTSTR Key)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020015, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Key);
}

void IHapp::Reinit()
{
	InvokeHelper(0x60020016, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

LPDISPATCH IHapp::CreateRouteTree(BSTR* propname, BSTR* routeid, BSTR* opsetid, short flag)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_PBSTR VTS_PBSTR VTS_PBSTR VTS_I2;
	InvokeHelper(0x60020018, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		propname, routeid, opsetid, flag);
	return result;
}

LPDISPATCH IHapp::GetEngine()
{
	LPDISPATCH result;
	InvokeHelper(0x6002001b, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

void IHapp::SaveLink(LPUNKNOWN pStrm, long format)
{
	static BYTE parms[] =
		VTS_UNKNOWN VTS_I4;
	InvokeHelper(0x6002001c, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pStrm, format);
}

void IHapp::LoadLink(LPUNKNOWN pStrm, long format)
{
	static BYTE parms[] =
		VTS_UNKNOWN VTS_I4;
	InvokeHelper(0x6002001d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pStrm, format);
}

void IHapp::Activate()
{
	InvokeHelper(0x6002001f, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHapp::SetCompat(long flag)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020020, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 flag);
}

void IHapp::Reconcile(long code)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x1, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 code);
}

void IHapp::AdviseParent(long dAdviseType, long lParam)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x60020022, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 dAdviseType, lParam);
}

void IHapp::Generate(LPCTSTR filename, long mode)
{
	static BYTE parms[] =
		VTS_BSTR VTS_I4;
	InvokeHelper(0x60020024, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, mode);
}

void IHapp::Readback(LPCTSTR filename, long mode)
{
	static BYTE parms[] =
		VTS_BSTR VTS_I4;
	InvokeHelper(0x60020025, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, mode);
}

void IHapp::SetParent(LPUNKNOWN pParentAdviseSink, long dwCookie)
{
	static BYTE parms[] =
		VTS_UNKNOWN VTS_I4;
	InvokeHelper(0x60020026, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pParentAdviseSink, dwCookie);
}

void IHapp::WriteArchive2(BSTR* filename, long bSaveChildren)
{
	static BYTE parms[] =
		VTS_PBSTR VTS_I4;
	InvokeHelper(0x60020027, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, bSaveChildren);
}

LPDISPATCH IHapp::GetNew3(const VARIANT& filename, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020028, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		&filename, &host_type, &node, &username, &password, &working_directory, &failmode);
	return result;
}

VARIANT IHapp::GetRestore2(LPCTSTR filename)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020029, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		filename);
	return result;
}

void IHapp::InitNew2(const VARIANT& notused, const VARIANT& notused2, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x6002002a, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &notused, &notused2, &host_type, &node, &username, &password, &working_directory, &failmode);
}

void IHapp::InitFromFile2(BSTR* filename, const VARIANT& readonly, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode)
{
	static BYTE parms[] =
		VTS_PBSTR VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x6002002b, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, &readonly, &host_type, &node, &username, &password, &working_directory, &failmode);
}

void IHapp::InitFromArchive2(BSTR* filename, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode)
{
	static BYTE parms[] =
		VTS_PBSTR VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x6002002c, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, &host_type, &node, &username, &password, &working_directory, &failmode);
}

void IHapp::Run2(const VARIANT& async)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x6002002d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &async);
}

void IHapp::InitFromTemplate2(BSTR* filename, const VARIANT& host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory, const VARIANT& failmode)
{
	static BYTE parms[] =
		VTS_PBSTR VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x6002002e, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, &host_type, &node, &username, &password, &working_directory, &failmode);
}

LPDISPATCH IHapp::GetLibRef()
{
	LPDISPATCH result;
	InvokeHelper(0x6002002f, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

long IHapp::GetSuppressDialogs()
{
	long result;
	InvokeHelper(0x60020030, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHapp::SetSuppressDialogs(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020030, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

void IHapp::Export(long reptype, LPCTSTR filename)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR;
	InvokeHelper(0x60020032, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 reptype, filename);
}

void IHapp::UIDisable(LPCTSTR Key)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020033, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Key);
}

LPUNKNOWN IHapp::GetEngineSimulation()
{
	LPUNKNOWN result;
	InvokeHelper(0x60020034, DISPATCH_PROPERTYGET, VT_UNKNOWN, (void*)&result, NULL);
	return result;
}

LPUNKNOWN IHapp::GetEngineServer()
{
	LPUNKNOWN result;
	InvokeHelper(0x60020035, DISPATCH_PROPERTYGET, VT_UNKNOWN, (void*)&result, NULL);
	return result;
}

void IHapp::InitFromXML(LPCTSTR argument)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020036, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 argument);
}


/////////////////////////////////////////////////////////////////////////////
// IHSelection properties

/////////////////////////////////////////////////////////////////////////////
// IHSelection operations

LPDISPATCH IHSelection::GetApplication()
{
	LPDISPATCH result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

LPDISPATCH IHSelection::GetParent()
{
	LPDISPATCH result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

long IHSelection::GetCount()
{
	long result;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

VARIANT IHSelection::GetItem(const VARIANT& loc_or_name)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x0, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		&loc_or_name);
	return result;
}

short IHSelection::GetItemType(const VARIANT& loc_or_name)
{
	short result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020004, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, parms,
		&loc_or_name);
	return result;
}

CString IHSelection::GetKey()
{
	CString result;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

CString IHSelection::GetLabel(long location)
{
	CString result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		location);
	return result;
}

void IHSelection::SetLabel(long location, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 location, lpszNewValue);
}

void IHSelection::Add(const VARIANT& loc_or_name, short type, const VARIANT& Item, LPDISPATCH owner, const VARIANT& scrollarea, const VARIANT& row, const VARIANT& col, const VARIANT& index, const VARIANT& xtwip, const VARIANT& ytwip)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_I2 VTS_VARIANT VTS_DISPATCH VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020008, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &loc_or_name, type, &Item, owner, &scrollarea, &row, &col, &index, &xtwip, &ytwip);
}

void IHSelection::Clear()
{
	InvokeHelper(0x60020009, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHSelection::Remove(const VARIANT& loc_or_name)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x6002000a, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &loc_or_name);
}

BOOL IHSelection::Find(const VARIANT& object)
{
	BOOL result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x6002000b, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		&object);
	return result;
}

void IHSelection::Copy()
{
	InvokeHelper(0x6002000c, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHSelection::CopyWithFormat()
{
	InvokeHelper(0x6002000d, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

BOOL IHSelection::Paste()
{
	BOOL result;
	InvokeHelper(0x6002000e, DISPATCH_METHOD, VT_BOOL, (void*)&result, NULL);
	return result;
}

BOOL IHSelection::PasteSpecial(const VARIANT& format, const VARIANT& link)
{
	BOOL result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x6002000f, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		&format, &link);
	return result;
}

void IHSelection::Lock(BOOL fLock)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020010, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 fLock);
}

void IHSelection::ClearOwned(LPDISPATCH owner)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x60020011, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 owner);
}

BOOL IHSelection::CanPrint()
{
	BOOL result;
	InvokeHelper(0x60020012, DISPATCH_METHOD, VT_BOOL, (void*)&result, NULL);
	return result;
}

VARIANT IHSelection::PreparePrinting()
{
	VARIANT result;
	InvokeHelper(0x60020013, DISPATCH_METHOD, VT_VARIANT, (void*)&result, NULL);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IHAPEngine properties

/////////////////////////////////////////////////////////////////////////////
// IHAPEngine operations

BOOL IHAPEngine::GetRunControl()
{
	BOOL result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHAPEngine::SetRunControl(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

BOOL IHAPEngine::GetControlPanel()
{
	BOOL result;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHAPEngine::SetControlPanel(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

BOOL IHAPEngine::GetReady()
{
	BOOL result;
	InvokeHelper(0x60020004, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHAPEngine::Step()
{
	InvokeHelper(0x60020006, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHAPEngine::Stop()
{
	InvokeHelper(0x60020007, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHAPEngine::Reinit(const VARIANT& object_type, const VARIANT& object_id)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020008, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &object_type, &object_id);
}

void IHAPEngine::MoveTo(long object_type, const VARIANT& object_id)
{
	static BYTE parms[] =
		VTS_I4 VTS_VARIANT;
	InvokeHelper(0x60020009, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 object_type, &object_id);
}

void IHAPEngine::ConnectionDialog()
{
	InvokeHelper(0x6002000a, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

BOOL IHAPEngine::Host(long host_type, const VARIANT& node, const VARIANT& username, const VARIANT& password, const VARIANT& working_directory)
{
	BOOL result;
	static BYTE parms[] =
		VTS_I4 VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x6002000b, DISPATCH_METHOD, VT_BOOL, (void*)&result, parms,
		host_type, &node, &username, &password, &working_directory);
	return result;
}

long IHAPEngine::GetHostCount()
{
	long result;
	InvokeHelper(0x6002000c, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

CString IHAPEngine::HostDescription(long host_type)
{
	CString result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6002000d, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms,
		host_type);
	return result;
}

void IHAPEngine::RunSettings()
{
	InvokeHelper(0x6002000e, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

CString IHAPEngine::GetEngineFilesSettings(long file)
{
	CString result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6002000f, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		file);
	return result;
}

void IHAPEngine::SetEngineFilesSettings(long file, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR;
	InvokeHelper(0x6002000f, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 file, lpszNewValue);
}

BOOL IHAPEngine::GetOptionSettings(long type)
{
	BOOL result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020011, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		type);
	return result;
}

void IHAPEngine::SetOptionSettings(long type, BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_BOOL;
	InvokeHelper(0x60020011, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 type, bNewValue);
}

void IHAPEngine::StopPoints()
{
	InvokeHelper(0x60020013, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

long IHAPEngine::GetStopPointCount()
{
	long result;
	InvokeHelper(0x60020014, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHAPEngine::GetStopPoint(long index, long* type, BSTR* object_id, long* before_or_after)
{
	static BYTE parms[] =
		VTS_I4 VTS_PI4 VTS_PBSTR VTS_PI4;
	InvokeHelper(0x60020015, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index, type, object_id, before_or_after);
}

void IHAPEngine::AddStopPoint(long type, LPCTSTR object_id, long before_or_after)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR VTS_I4;
	InvokeHelper(0x60020016, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 type, object_id, before_or_after);
}

void IHAPEngine::DeleteStopPoint(long index)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020017, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index);
}

void IHAPEngine::ClearStopPoints()
{
	InvokeHelper(0x60020018, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

BOOL IHAPEngine::GetIsRunning()
{
	BOOL result;
	InvokeHelper(0x6002001a, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHAPEngine::Run2(const VARIANT& async)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x6002001b, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &async);
}

void IHAPEngine::ExportReport(LPCTSTR filename, long contents, const VARIANT& object_id)
{
	static BYTE parms[] =
		VTS_BSTR VTS_I4 VTS_VARIANT;
	InvokeHelper(0x6002001d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, contents, &object_id);
}

void IHAPEngine::SynchronizeEO(const VARIANT& reserved)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x6002001e, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &reserved);
}

void IHAPEngine::ReinitializeEO(const VARIANT& reserved)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x6002001f, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &reserved);
}


/////////////////////////////////////////////////////////////////////////////
// IHAPLibRef properties

/////////////////////////////////////////////////////////////////////////////
// IHAPLibRef operations

short IHAPLibRef::GetCountLibs()
{
	short result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

CString IHAPLibRef::GetLibraryName(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

CString IHAPLibRef::GetLibraryPath(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

CString IHAPLibRef::InsertLibrary(LPCTSTR path, short location)
{
	CString result;
	static BYTE parms[] =
		VTS_BSTR VTS_I2;
	InvokeHelper(0x60020003, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms,
		path, location);
	return result;
}

void IHAPLibRef::RemoveLibrary(short location)
{
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x60020004, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 location);
}

void IHAPLibRef::MoveLibrary(short fromloc, short toloc)
{
	static BYTE parms[] =
		VTS_I2 VTS_I2;
	InvokeHelper(0x60020005, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 fromloc, toloc);
}

CString IHAPLibRef::GetCategoryName(short index)
{
	CString result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

short IHAPLibRef::GetCategorySelected(LPCTSTR Name)
{
	short result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020007, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, parms,
		Name);
	return result;
}

void IHAPLibRef::SetCategorySelected(LPCTSTR Name, short nNewValue)
{
	static BYTE parms[] =
		VTS_BSTR VTS_I2;
	InvokeHelper(0x60020007, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 Name, nNewValue);
}

short IHAPLibRef::GetCategoryLocSelected(short index)
{
	short result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x60020009, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, parms,
		index);
	return result;
}

void IHAPLibRef::SetCategoryLocSelected(short index, short nNewValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_I2;
	InvokeHelper(0x60020009, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, nNewValue);
}

void IHAPLibRef::MoveCategory(short fromloc, short toloc)
{
	static BYTE parms[] =
		VTS_I2 VTS_I2;
	InvokeHelper(0x6002000b, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 fromloc, toloc);
}

void IHAPLibRef::SetLibraryActive(LPCTSTR displayname)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x6002000d, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 displayname);
}


/////////////////////////////////////////////////////////////////////////////
// IHNodeCol properties

/////////////////////////////////////////////////////////////////////////////
// IHNodeCol operations

LPDISPATCH IHNodeCol::GetApplication()
{
	LPDISPATCH result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

LPDISPATCH IHNodeCol::GetParent()
{
	LPDISPATCH result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

long IHNodeCol::GetCount()
{
	long result;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

long IHNodeCol::GetRowCount(long Dimension)
{
	long result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, parms,
		Dimension);
	return result;
}

long IHNodeCol::GetDimension()
{
	long result;
	InvokeHelper(0x60020004, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

BOOL IHNodeCol::GetIsNamedDimension(const VARIANT& Dimension)
{
	BOOL result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		&Dimension);
	return result;
}

LPDISPATCH IHNodeCol::GetItem(const VARIANT& loc_or_name, const VARIANT& loc_or_name2, const VARIANT& loc_or_name3, const VARIANT& loc_or_name4, const VARIANT& loc_or_name5)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x0, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		&loc_or_name, &loc_or_name2, &loc_or_name3, &loc_or_name4, &loc_or_name5);
	return result;
}

CString IHNodeCol::GetItemName(long location, const VARIANT& Dimension, const VARIANT& force)
{
	CString result;
	static BYTE parms[] =
		VTS_I4 VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020007, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		location, &Dimension, &force);
	return result;
}

void IHNodeCol::SetItemName(long location, const VARIANT& Dimension, const VARIANT& force, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_VARIANT VTS_VARIANT VTS_BSTR;
	InvokeHelper(0x60020007, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 location, &Dimension, &force, lpszNewValue);
}

CString IHNodeCol::GetLabel(long Dimension, long location, const VARIANT& force)
{
	CString result;
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_VARIANT;
	InvokeHelper(0x60020009, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		Dimension, location, &force);
	return result;
}

void IHNodeCol::SetLabel(long Dimension, long location, const VARIANT& force, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_VARIANT VTS_BSTR;
	InvokeHelper(0x60020009, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 Dimension, location, &force, lpszNewValue);
}

LPDISPATCH IHNodeCol::GetLabelNode(long Dimension, long location, VARIANT* Label)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_PVARIANT;
	InvokeHelper(0x6002000b, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		Dimension, location, Label);
	return result;
}

long IHNodeCol::GetLabelLocation(LPCTSTR Label, long Dimension)
{
	long result;
	static BYTE parms[] =
		VTS_BSTR VTS_I4;
	InvokeHelper(0x6002000c, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, parms,
		Label, Dimension);
	return result;
}

VARIANT IHNodeCol::GetLabelAttribute(long Dimension, long location, short attrnum, const VARIANT& force)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_I2 VTS_VARIANT;
	InvokeHelper(0x6002000d, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		Dimension, location, attrnum, &force);
	return result;
}

void IHNodeCol::SetLabelAttribute(long Dimension, long location, short attrnum, const VARIANT& force, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_I2 VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x6002000d, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 Dimension, location, attrnum, &force, &newValue);
}

short IHNodeCol::GetLabelAttributeType(long Dimension, long location, short attrnum)
{
	short result;
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_I2;
	InvokeHelper(0x6002000f, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, parms,
		Dimension, location, attrnum);
	return result;
}

LPDISPATCH IHNodeCol::Add(const VARIANT& loc_or_name, const VARIANT& loc_or_name2, const VARIANT& loc_or_name3, const VARIANT& loc_or_name4, const VARIANT& loc_or_name5)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020011, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		&loc_or_name, &loc_or_name2, &loc_or_name3, &loc_or_name4, &loc_or_name5);
	return result;
}

void IHNodeCol::Insert(LPDISPATCH element, const VARIANT& loc_or_name, const VARIANT& loc_or_name2, const VARIANT& loc_or_name3, const VARIANT& loc_or_name4, const VARIANT& loc_or_name5)
{
	static BYTE parms[] =
		VTS_DISPATCH VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020012, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 element, &loc_or_name, &loc_or_name2, &loc_or_name3, &loc_or_name4, &loc_or_name5);
}

LPDISPATCH IHNodeCol::Remove(const VARIANT& loc_or_name, const VARIANT& loc_or_name2, const VARIANT& loc_or_name3, const VARIANT& loc_or_name4, const VARIANT& loc_or_name5)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020013, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		&loc_or_name, &loc_or_name2, &loc_or_name3, &loc_or_name4, &loc_or_name5);
	return result;
}

void IHNodeCol::InsertRow(long Dimension, long location)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x60020014, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Dimension, location);
}

void IHNodeCol::RemoveRow(long Dimension, long location)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x60020015, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Dimension, location);
}

CString IHNodeCol::GetDimensionName(long Dimension)
{
	CString result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020016, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		Dimension);
	return result;
}

void IHNodeCol::Reorder(const VARIANT& loc_or_name, short dir)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_I2;
	InvokeHelper(0x60020017, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &loc_or_name, dir);
}


/////////////////////////////////////////////////////////////////////////////
// IScrollAreaInfo properties

/////////////////////////////////////////////////////////////////////////////
// IScrollAreaInfo operations

LPDISPATCH IScrollAreaInfo::GetNodeAt(long row, long col, short index)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_I2;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		row, col, index);
	return result;
}

void IScrollAreaInfo::GetMaximum(long* pRow, short* pRowExt, long* pCol, short* pColExt)
{
	static BYTE parms[] =
		VTS_PI4 VTS_PI2 VTS_PI4 VTS_PI2;
	InvokeHelper(0x60020001, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pRow, pRowExt, pCol, pColExt);
}


/////////////////////////////////////////////////////////////////////////////
// IHPlotVal properties

/////////////////////////////////////////////////////////////////////////////
// IHPlotVal operations

void IHPlotVal::SetContext(LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

LPDISPATCH IHPlotVal::GetContext()
{
	LPDISPATCH result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

CString IHPlotVal::GetTitle()
{
	CString result;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

CString IHPlotVal::GetTitleInt()
{
	CString result;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetTitle(LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lpszNewValue);
}

CString IHPlotVal::GetXAxisTitle()
{
	CString result;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

CString IHPlotVal::GetXAxisTitleInt()
{
	CString result;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetXAxisTitle(LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lpszNewValue);
}

CString IHPlotVal::GetYAxisTitle(const VARIANT& nAxis)
{
	CString result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020008, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		&nAxis);
	return result;
}

CString IHPlotVal::GetYAxisTitleInt(const VARIANT& nAxis)
{
	CString result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020009, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		&nAxis);
	return result;
}

void IHPlotVal::SetYAxisTitle(const VARIANT& nAxis, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_BSTR;
	InvokeHelper(0x60020008, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 &nAxis, lpszNewValue);
}

CString IHPlotVal::GetLabel(long index)
{
	CString result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6002000b, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

CString IHPlotVal::GetLabelInt(long index)
{
	CString result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6002000c, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

void IHPlotVal::SetLabel(long index, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR;
	InvokeHelper(0x6002000b, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, lpszNewValue);
}

long IHPlotVal::GetNPoints()
{
	long result;
	InvokeHelper(0x6002000e, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetNPoints(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6002000e, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

long IHPlotVal::GetNVariables()
{
	long result;
	InvokeHelper(0x60020010, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetNVariables(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020010, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

long IHPlotVal::GetNXVariables()
{
	long result;
	InvokeHelper(0x60020012, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetNXVariables(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020012, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

double IHPlotVal::GetXDataValue(long index, long datano)
{
	double result;
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x60020014, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, parms,
		index, datano);
	return result;
}

void IHPlotVal::SetXData(long index, long datano, short attr, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_I2 VTS_VARIANT;
	InvokeHelper(0x60020015, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, datano, attr, &newValue);
}

double IHPlotVal::GetYDataValue(long index, long datano)
{
	double result;
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x60020016, DISPATCH_PROPERTYGET, VT_R8, (void*)&result, parms,
		index, datano);
	return result;
}

void IHPlotVal::SetYData(long index, long datano, short attr, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_I2 VTS_VARIANT;
	InvokeHelper(0x60020017, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, datano, attr, &newValue);
}

void IHPlotVal::GetXData(long index, long datano, short* attr, VARIANT* Value)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_PI2 VTS_PVARIANT;
	InvokeHelper(0x60020018, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index, datano, attr, Value);
}

void IHPlotVal::GetYData(long index, long datano, short* attr, VARIANT* Value)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4 VTS_PI2 VTS_PVARIANT;
	InvokeHelper(0x60020019, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 index, datano, attr, Value);
}

void IHPlotVal::Clear()
{
	InvokeHelper(0x6002001a, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

BOOL IHPlotVal::GetSwapAxises()
{
	BOOL result;
	InvokeHelper(0x6002001b, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetSwapAxises(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x6002001b, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

BOOL IHPlotVal::GetReverseXAxis()
{
	BOOL result;
	InvokeHelper(0x6002001d, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetReverseXAxis(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x6002001d, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

BOOL IHPlotVal::GetAddTimeStamp()
{
	BOOL result;
	InvokeHelper(0x6002001f, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetAddTimeStamp(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x6002001f, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

BOOL IHPlotVal::GetShowLegend()
{
	BOOL result;
	InvokeHelper(0x60020021, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetShowLegend(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020021, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

long IHPlotVal::GetPlotType()
{
	long result;
	InvokeHelper(0x60020023, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetPlotType(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020023, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

BOOL IHPlotVal::GetParametric()
{
	BOOL result;
	InvokeHelper(0x60020025, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetParametric(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020025, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

CString IHPlotVal::GetZAxisTitle()
{
	CString result;
	InvokeHelper(0x60020027, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

CString IHPlotVal::GetZAxisTitleInt()
{
	CString result;
	InvokeHelper(0x60020028, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetZAxisTitle(LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020027, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lpszNewValue);
}

BOOL IHPlotVal::GetLive()
{
	BOOL result;
	InvokeHelper(0x6002002a, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetLive(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x6002002a, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

long IHPlotVal::GetAnnotationCount()
{
	long result;
	InvokeHelper(0x6002002d, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

short IHPlotVal::GetGridType()
{
	short result;
	InvokeHelper(0x6002002f, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetGridType(short nNewValue)
{
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x6002002f, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

short IHPlotVal::GetMarkerSize()
{
	short result;
	InvokeHelper(0x60020031, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetMarkerSize(short nNewValue)
{
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x60020031, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

BOOL IHPlotVal::GetShowDiagonalLine()
{
	BOOL result;
	InvokeHelper(0x60020033, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetShowDiagonalLine(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020033, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

short IHPlotVal::GetLineStyle()
{
	short result;
	InvokeHelper(0x60020035, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetLineStyle(short nNewValue)
{
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x60020035, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

BOOL IHPlotVal::GetXDataInteger(long index)
{
	BOOL result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020037, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		index);
	return result;
}

BOOL IHPlotVal::GetShowZeroLine()
{
	BOOL result;
	InvokeHelper(0x60020038, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetShowZeroLine(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020038, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

long IHPlotVal::GetAxisMap(long index)
{
	long result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6002003a, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, parms,
		index);
	return result;
}

void IHPlotVal::SetAxisMap(long index, long nNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x6002003a, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, nNewValue);
}

long IHPlotVal::GetCurveStyle(long index)
{
	long result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6002003c, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, parms,
		index);
	return result;
}

void IHPlotVal::SetCurveStyle(long index, long nNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x6002003c, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, nNewValue);
}

void IHPlotVal::RemoveAnnotationText()
{
	InvokeHelper(0x6002003e, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

BOOL IHPlotVal::GetSquarePlot()
{
	BOOL result;
	InvokeHelper(0x6002003f, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetSquarePlot(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x6002003f, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

BOOL IHPlotVal::GetCanLive()
{
	BOOL result;
	InvokeHelper(0x60020041, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IHPlotVal::SetCanLive(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020041, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

long IHPlotVal::GetAxisScale(long VarNo)
{
	long result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020043, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, parms,
		VarNo);
	return result;
}

void IHPlotVal::SetAxisScale(long VarNo, long nNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_I4;
	InvokeHelper(0x60020043, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 VarNo, nNewValue);
}


/////////////////////////////////////////////////////////////////////////////
// IAPPDF properties

/////////////////////////////////////////////////////////////////////////////
// IAPPDF operations

void IAPPDF::GetAPPDF(LPCTSTR filename, long flags)
{
	static BYTE parms[] =
		VTS_BSTR VTS_I4;
	InvokeHelper(0x60020000, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, flags);
}


/////////////////////////////////////////////////////////////////////////////
// IHAPHandle properties

/////////////////////////////////////////////////////////////////////////////
// IHAPHandle operations

long IHAPHandle::GetMainHWnd()
{
	long result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

long IHAPHandle::GetProcessHandle()
{
	long result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAPropData properties

/////////////////////////////////////////////////////////////////////////////
// IAPropData operations

void IAPropData::GetAvailableDatabanks(long* plNumDatabanks, VARIANT* pvtDatabankArray)
{
	static BYTE parms[] =
		VTS_PI4 VTS_PVARIANT;
	InvokeHelper(0x60020000, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 plNumDatabanks, pvtDatabankArray);
}

void IAPropData::GetDefaultDatabanks(long* plNumDatabanks, VARIANT* pvtDatabankArray)
{
	static BYTE parms[] =
		VTS_PI4 VTS_PVARIANT;
	InvokeHelper(0x60020001, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 plNumDatabanks, pvtDatabankArray);
}

void IAPropData::SetQueryForComponents(const VARIANT& vtDatabanksArray, LPCTSTR bstrMatchNameAlias, long bMatchAlternate, long bMatchStringBeginOnly, LPCTSTR bstrCompClass, LPCTSTR bstrCASRN, double dblMWfrom, double dblMWto, double dblBPfrom, 
		double dblBPto, LPCTSTR bstrBPUnit)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_BSTR VTS_I4 VTS_I4 VTS_BSTR VTS_BSTR VTS_R8 VTS_R8 VTS_R8 VTS_R8 VTS_BSTR;
	InvokeHelper(0x60020002, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &vtDatabanksArray, bstrMatchNameAlias, bMatchAlternate, bMatchStringBeginOnly, bstrCompClass, bstrCASRN, dblMWfrom, dblMWto, dblBPfrom, dblBPto, bstrBPUnit);
}

void IAPropData::FetchNextComponent(long bInit, BSTR* bstrAlias, BSTR* bstrName, double* dblBP, double* dblMW, BSTR* bstrDatabank, BSTR* bstrCASRN, BSTR* bstrCompClass, BSTR* bstrSynonym)
{
	static BYTE parms[] =
		VTS_I4 VTS_PBSTR VTS_PBSTR VTS_PR8 VTS_PR8 VTS_PBSTR VTS_PBSTR VTS_PBSTR VTS_PBSTR;
	InvokeHelper(0x60020003, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 bInit, bstrAlias, bstrName, dblBP, dblMW, bstrDatabank, bstrCASRN, bstrCompClass, bstrSynonym);
}


/////////////////////////////////////////////////////////////////////////////
// IAPPasteItems properties

/////////////////////////////////////////////////////////////////////////////
// IAPPasteItems operations

LPDISPATCH IAPPasteItems::GetItem(const VARIANT& index)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x0, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		&index);
	return result;
}

LPUNKNOWN IAPPasteItems::GetEnum()
{
	LPUNKNOWN result;
	InvokeHelper(0xfffffffc, DISPATCH_PROPERTYGET, VT_UNKNOWN, (void*)&result, NULL);
	return result;
}

long IAPPasteItems::GetCount()
{
	long result;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAPPasteItem properties

/////////////////////////////////////////////////////////////////////////////
// IAPPasteItem operations

CString IAPPasteItem::GetDisplayname()
{
	CString result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IAPPasteItem::SetId(LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lpszNewValue);
}

CString IAPPasteItem::GetId()
{
	CString result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IAPPasteItem::SetDeleted(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

BOOL IAPPasteItem::GetDeleted()
{
	BOOL result;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

long IAPPasteItem::GetPathLen()
{
	long result;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

CString IAPPasteItem::GetPath(long index)
{
	CString result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

BOOL IAPPasteItem::GetHasChildren()
{
	BOOL result;
	InvokeHelper(0x60020007, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

LPDISPATCH IAPPasteItem::GetItems()
{
	LPDISPATCH result;
	InvokeHelper(0x60020008, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IHNode2 properties

/////////////////////////////////////////////////////////////////////////////
// IHNode2 operations

LPDISPATCH IHNode2::GetApplication()
{
	LPDISPATCH result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

LPDISPATCH IHNode2::GetParent()
{
	LPDISPATCH result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

CString IHNode2::GetName(const VARIANT& force)
{
	CString result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, parms,
		&force);
	return result;
}

void IHNode2::SetName(const VARIANT& force, LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_BSTR;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 &force, lpszNewValue);
}

long IHNode2::GetDimension()
{
	long result;
	InvokeHelper(0x60020004, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

short IHNode2::GetValueType()
{
	short result;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, NULL);
	return result;
}

VARIANT IHNode2::GetValue(const VARIANT& force)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		&force);
	return result;
}

void IHNode2::SetValue(const VARIANT& force, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 &force, &newValue);
}

void IHNode2::SetValueAndUnit(const VARIANT& Value, short unitcol, const VARIANT& force)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_I2 VTS_VARIANT;
	InvokeHelper(0x60020008, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &Value, unitcol, &force);
}

void IHNode2::SetValueUnitAndBasis(const VARIANT& Value, short unitcol, LPCTSTR basis, const VARIANT& force)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_I2 VTS_BSTR VTS_VARIANT;
	InvokeHelper(0x60020009, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &Value, unitcol, basis, &force);
}

BOOL IHNode2::GetHasAttribute(short attrnumber)
{
	BOOL result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x6002000a, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		attrnumber);
	return result;
}

short IHNode2::GetAttributeType(short attrnumber)
{
	short result;
	static BYTE parms[] =
		VTS_I2;
	InvokeHelper(0x6002000b, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, parms,
		attrnumber);
	return result;
}

VARIANT IHNode2::GetAttributeValue(short attrnumber, const VARIANT& force)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_I2 VTS_VARIANT;
	InvokeHelper(0x6002000c, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		attrnumber, &force);
	return result;
}

void IHNode2::SetAttributeValue(short attrnumber, const VARIANT& force, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_I2 VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x6002000c, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 attrnumber, &force, &newValue);
}

BOOL IHNode2::GetHasClassAttribute(LPCTSTR classid)
{
	BOOL result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x6002000e, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		classid);
	return result;
}

short IHNode2::GetClassAttributeType(LPCTSTR classid)
{
	short result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x6002000f, DISPATCH_PROPERTYGET, VT_I2, (void*)&result, parms,
		classid);
	return result;
}

VARIANT IHNode2::GetClassAttributeValue(LPCTSTR classid, const VARIANT& force)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_BSTR VTS_VARIANT;
	InvokeHelper(0x60020010, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		classid, &force);
	return result;
}

void IHNode2::SetClassAttributeValue(LPCTSTR classid, const VARIANT& force, const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_BSTR VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020010, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 classid, &force, &newValue);
}

LPDISPATCH IHNode2::GetElements()
{
	LPDISPATCH result;
	InvokeHelper(0x60020012, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

void IHNode2::Delete()
{
	InvokeHelper(0x60020014, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHNode2::RemoveAll()
{
	InvokeHelper(0x60020015, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHNode2::AddClassAttribute(LPCTSTR classid, short type)
{
	static BYTE parms[] =
		VTS_BSTR VTS_I2;
	InvokeHelper(0x60020016, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 classid, type);
}

void IHNode2::DeleteClassAttribute(LPCTSTR classid)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020017, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 classid);
}

void IHNode2::PrintUseful(short append, LPCTSTR filename)
{
	static BYTE parms[] =
		VTS_I2 VTS_BSTR;
	InvokeHelper(0x6002001a, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 append, filename);
}

LPDISPATCH IHNode2::FindNode(LPCTSTR path)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x6002001b, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		path);
	return result;
}

CString IHNode2::NextIncomplete(VARIANT* code)
{
	CString result;
	static BYTE parms[] =
		VTS_PVARIANT;
	InvokeHelper(0x60020020, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms,
		code);
	return result;
}

CString IHNode2::BrowseNext(short direction, short io)
{
	CString result;
	static BYTE parms[] =
		VTS_I2 VTS_I2;
	InvokeHelper(0x60020021, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms,
		direction, io);
	return result;
}

void IHNode2::Hide(LPCTSTR Name)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020023, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

void IHNode2::Reveal(LPCTSTR Name)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020024, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

void IHNode2::Clear()
{
	InvokeHelper(0x60020025, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHNode2::NewChild(BSTR* Name)
{
	static BYTE parms[] =
		VTS_PBSTR;
	InvokeHelper(0x60020029, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

void IHNode2::RenameChild(BSTR* Name)
{
	static BYTE parms[] =
		VTS_PBSTR;
	InvokeHelper(0x6002002a, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

CString IHNode2::GetUnitString()
{
	CString result;
	InvokeHelper(0x6002002d, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

VARIANT IHNode2::GetValueForUnit(short unitrow, short unitcol)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_I2 VTS_I2;
	InvokeHelper(0x6002002e, DISPATCH_PROPERTYGET, VT_VARIANT, (void*)&result, parms,
		unitrow, unitcol);
	return result;
}

void IHNode2::Reconcile(long code)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020031, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 code);
}

void IHNode2::NewID(BSTR* Name)
{
	static BYTE parms[] =
		VTS_PBSTR;
	InvokeHelper(0x60020032, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

void IHNode2::Export(LPCTSTR filename)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020033, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename);
}

void IHNode2::Clear2(long code)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60030000, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 code);
}


/////////////////////////////////////////////////////////////////////////////
// IAPHappEvents properties

/////////////////////////////////////////////////////////////////////////////
// IAPHappEvents operations

void IAPHappEvents::OnDialogSuppressed(LPCTSTR msg, LPCTSTR result)
{
	static BYTE parms[] =
		VTS_BSTR VTS_BSTR;
	InvokeHelper(0x64, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 msg, result);
}

void IAPHappEvents::OnControlPanelMessage(long Clear, LPCTSTR msg)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR;
	InvokeHelper(0x65, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Clear, msg);
}

void IAPHappEvents::OnGUIClosing()
{
	InvokeHelper(0x66, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAPHappEvents::OnInClosing(long Clear, LPCTSTR msg)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR;
	InvokeHelper(0x67, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Clear, msg);
}

void IAPHappEvents::OnDataChanged(LPDISPATCH pObj)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x68, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pObj);
}

void IAPHappEvents::OnUpdateMessage(long msg_code, LPCTSTR msg_hint, LPDISPATCH msg_object, long source_handle)
{
	static BYTE parms[] =
		VTS_I4 VTS_BSTR VTS_DISPATCH VTS_I4;
	InvokeHelper(0x69, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 msg_code, msg_hint, msg_object, source_handle);
}

void IAPHappEvents::OnEngineCommandCompleted()
{
	InvokeHelper(0x6a, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAPHappEvents::OnBeforeCalculate(BOOL IsStep, BOOL* Cancel)
{
	static BYTE parms[] =
		VTS_BOOL VTS_PBOOL;
	InvokeHelper(0x6b, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 IsStep, Cancel);
}

void IAPHappEvents::OnBeforeSave(LPCTSTR filename, BOOL* Cancel)
{
	static BYTE parms[] =
		VTS_BSTR VTS_PBOOL;
	InvokeHelper(0x6c, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, Cancel);
}

void IAPHappEvents::OnCalculationCompleted()
{
	InvokeHelper(0x6d, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAPHappEvents::OnCalculationStopped()
{
	InvokeHelper(0x6e, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}


/////////////////////////////////////////////////////////////////////////////
// IHComposite properties

/////////////////////////////////////////////////////////////////////////////
// IHComposite operations

void IHComposite::AddChild(LPCTSTR Name, LPDISPATCH child)
{
	static BYTE parms[] =
		VTS_BSTR VTS_DISPATCH;
	InvokeHelper(0x60020000, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name, child);
}

void IHComposite::RemoveChild(LPCTSTR Name)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020001, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 Name);
}

LPDISPATCH IHComposite::GetChild(long index)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020002, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		index);
	return result;
}

CString IHComposite::GetChildName(long index)
{
	CString result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020003, DISPATCH_METHOD, VT_BSTR, (void*)&result, parms,
		index);
	return result;
}

long IHComposite::GetChildCount()
{
	long result;
	InvokeHelper(0x60020004, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHComposite::DumpToDataBase(LPCTSTR filename, LPCTSTR tablename, long create)
{
	static BYTE parms[] =
		VTS_BSTR VTS_BSTR VTS_I4;
	InvokeHelper(0x60020005, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename, tablename, create);
}


/////////////////////////////////////////////////////////////////////////////
// IHWizardPlot properties

/////////////////////////////////////////////////////////////////////////////
// IHWizardPlot operations

LPDISPATCH IHWizardPlot::GetPlotVal()
{
	LPDISPATCH result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

long IHWizardPlot::GetBasis()
{
	long result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHWizardPlot::SetBasis(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

long IHWizardPlot::GetUnitrow(const VARIANT& curve)
{
	long result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, parms,
		&curve);
	return result;
}

void IHWizardPlot::SetUnitrow(const VARIANT& curve, long nNewValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_I4;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 &curve, nNewValue);
}

long IHWizardPlot::GetUnitcol(const VARIANT& curve)
{
	long result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, parms,
		&curve);
	return result;
}

void IHWizardPlot::SetUnitcol(const VARIANT& curve, long nNewValue)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_I4;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 &curve, nNewValue);
}

void IHWizardPlot::SetIndependentVariable(LPDISPATCH pContext, LPCTSTR indepvar)
{
	static BYTE parms[] =
		VTS_DISPATCH VTS_BSTR;
	InvokeHelper(0x60020007, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pContext, indepvar);
}

void IHWizardPlot::AddDependentVariable(LPDISPATCH pContext, LPCTSTR depvar)
{
	static BYTE parms[] =
		VTS_DISPATCH VTS_BSTR;
	InvokeHelper(0x60020008, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pContext, depvar);
}

void IHWizardPlot::RemoveDependentVariable(LPDISPATCH pContext, LPCTSTR depvar)
{
	static BYTE parms[] =
		VTS_DISPATCH VTS_BSTR;
	InvokeHelper(0x60020009, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pContext, depvar);
}

void IHWizardPlot::RemoveAllDependentVariable()
{
	InvokeHelper(0x6002000a, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

BOOL IHWizardPlot::GetHasDependentVariable(LPDISPATCH pContext, LPCTSTR depvar)
{
	BOOL result;
	static BYTE parms[] =
		VTS_DISPATCH VTS_BSTR;
	InvokeHelper(0x6002000b, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, parms,
		pContext, depvar);
	return result;
}

void IHWizardPlot::AddVariablePair(LPDISPATCH pContext, LPCTSTR indepvar, LPCTSTR depvar, short sort)
{
	static BYTE parms[] =
		VTS_DISPATCH VTS_BSTR VTS_BSTR VTS_I2;
	InvokeHelper(0x6002000c, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pContext, indepvar, depvar, sort);
}

LPDISPATCH IHWizardPlot::GetWizardData(long Step)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6002000d, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		Step);
	return result;
}

void IHWizardPlot::SetWizardData(long Step, LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_DISPATCH;
	InvokeHelper(0x6002000d, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 Step, newValue);
}

void IHWizardPlot::Clear()
{
	InvokeHelper(0x6002000f, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

long IHWizardPlot::GetErrorNumber()
{
	long result;
	InvokeHelper(0x60020010, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHWizardPlot::SetErrorStatus(long index, BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_I4 VTS_BOOL;
	InvokeHelper(0x60020011, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 index, bNewValue);
}

void IHWizardPlot::SetRefreshCallBack(LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020012, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lpszNewValue);
}

CString IHWizardPlot::GetRefreshCallBack()
{
	CString result;
	InvokeHelper(0x60020012, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IHWizardPlot::GetIndependentVariable(BSTR* indepvar)
{
	static BYTE parms[] =
		VTS_PBSTR;
	InvokeHelper(0x60020014, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 indepvar);
}

void IHWizardPlot::SetAddToPlot(LPDISPATCH newValue)
{
	static BYTE parms[] =
		VTS_DISPATCH;
	InvokeHelper(0x60020017, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 newValue);
}

LPDISPATCH IHWizardPlot::GetAddToPlot()
{
	LPDISPATCH result;
	InvokeHelper(0x60020017, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

long IHWizardPlot::GetXUnitRow()
{
	long result;
	InvokeHelper(0x60020019, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

long IHWizardPlot::GetXUnitCol()
{
	long result;
	InvokeHelper(0x6002001a, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

void IHWizardPlot::SetXUnitCol(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x6002001a, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}


/////////////////////////////////////////////////////////////////////////////
// IHAdhocPlot properties

/////////////////////////////////////////////////////////////////////////////
// IHAdhocPlot operations

LPDISPATCH IHAdhocPlot::GetPlotVal()
{
	LPDISPATCH result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

void IHAdhocPlot::ReadyToPlot()
{
	InvokeHelper(0x60020001, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHAdhocPlot::SetIndependentVariable()
{
	InvokeHelper(0x60020002, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHAdhocPlot::SetDependentVariable()
{
	InvokeHelper(0x60020003, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHAdhocPlot::SetParametricVariable()
{
	InvokeHelper(0x60020004, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IHAdhocPlot::Refresh()
{
	InvokeHelper(0x60020005, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}


/////////////////////////////////////////////////////////////////////////////
// IMMControlVerb properties

/////////////////////////////////////////////////////////////////////////////
// IMMControlVerb operations

void IMMControlVerb::EnumVerbs(VARIANT* pEnumOleVerb)
{
	static BYTE parms[] =
		VTS_PVARIANT;
	InvokeHelper(0x60020000, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 pEnumOleVerb);
}

void IMMControlVerb::DoVerb(long iVerb)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020001, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 iVerb);
}


/////////////////////////////////////////////////////////////////////////////
// IHappServiceProvider properties

/////////////////////////////////////////////////////////////////////////////
// IHappServiceProvider operations

LPUNKNOWN IHappServiceProvider::GetService(LPCTSTR bService)
{
	LPUNKNOWN result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020000, DISPATCH_METHOD, VT_UNKNOWN, (void*)&result, parms,
		bService);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAPConflict properties

/////////////////////////////////////////////////////////////////////////////
// IAPConflict operations

CString IAPConflict::GetDisplayname()
{
	CString result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IAPConflict::SetId(LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lpszNewValue);
}

CString IAPConflict::GetId()
{
	CString result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IAPConflict::SetMerge(BOOL bNewValue)
{
	static BYTE parms[] =
		VTS_BOOL;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 bNewValue);
}

BOOL IAPConflict::GetMerge()
{
	BOOL result;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYGET, VT_BOOL, (void*)&result, NULL);
	return result;
}

void IAPConflict::SetAction(long nNewValue)
{
	static BYTE parms[] =
		VTS_I4;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 nNewValue);
}

long IAPConflict::GetAction()
{
	long result;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAPConflicts properties

/////////////////////////////////////////////////////////////////////////////
// IAPConflicts operations

LPDISPATCH IAPConflicts::Item(const VARIANT& index)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x0, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		&index);
	return result;
}

LPUNKNOWN IAPConflicts::GetEnum()
{
	LPUNKNOWN result;
	InvokeHelper(0xfffffffc, DISPATCH_PROPERTYGET, VT_UNKNOWN, (void*)&result, NULL);
	return result;
}

void IAPConflicts::Clear()
{
	InvokeHelper(0x60020002, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

long IAPConflicts::GetCount()
{
	long result;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAPGroup properties

/////////////////////////////////////////////////////////////////////////////
// IAPGroup operations

void IAPGroup::Clear()
{
	InvokeHelper(0x60020000, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}

void IAPGroup::Add(const VARIANT& Item)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020001, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &Item);
}

void IAPGroup::Remove(const VARIANT& Item)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020002, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &Item);
}

VARIANT IAPGroup::Item(const VARIANT& index)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x0, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&index);
	return result;
}

LPUNKNOWN IAPGroup::Get_NewEnum()
{
	LPUNKNOWN result;
	InvokeHelper(0xfffffffc, DISPATCH_PROPERTYGET, VT_UNKNOWN, (void*)&result, NULL);
	return result;
}

long IAPGroup::GetCount()
{
	long result;
	InvokeHelper(0x60020005, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}

LPUNKNOWN IAPGroup::GetDataObject()
{
	LPUNKNOWN result;
	InvokeHelper(0x60020006, DISPATCH_PROPERTYGET, VT_UNKNOWN, (void*)&result, NULL);
	return result;
}

void IAPGroup::Export(LPCTSTR filename)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020007, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename);
}

void IAPGroup::CopyToClipboard()
{
	InvokeHelper(0x60020008, DISPATCH_METHOD, VT_EMPTY, NULL, NULL);
}


/////////////////////////////////////////////////////////////////////////////
// IAPPasteParam properties

/////////////////////////////////////////////////////////////////////////////
// IAPPasteParam operations

CString IAPPasteParam::GetId()
{
	CString result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IAPPasteParam::SetLabel(LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lpszNewValue);
}

CString IAPPasteParam::GetLabel()
{
	CString result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}

void IAPPasteParam::SetValue(LPCTSTR lpszNewValue)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 lpszNewValue);
}

CString IAPPasteParam::GetValue()
{
	CString result;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYGET, VT_BSTR, (void*)&result, NULL);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAPPasteParams properties

/////////////////////////////////////////////////////////////////////////////
// IAPPasteParams operations

LPDISPATCH IAPPasteParams::GetItem(const VARIANT& index)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x0, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, parms,
		&index);
	return result;
}

LPUNKNOWN IAPPasteParams::GetEnum()
{
	LPUNKNOWN result;
	InvokeHelper(0xfffffffc, DISPATCH_PROPERTYGET, VT_UNKNOWN, (void*)&result, NULL);
	return result;
}

long IAPPasteParams::GetCount()
{
	long result;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_I4, (void*)&result, NULL);
	return result;
}


/////////////////////////////////////////////////////////////////////////////
// IAPPasteBuffer properties

/////////////////////////////////////////////////////////////////////////////
// IAPPasteBuffer operations

LPDISPATCH IAPPasteBuffer::GetItems()
{
	LPDISPATCH result;
	InvokeHelper(0x60020000, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

LPDISPATCH IAPPasteBuffer::GetParams()
{
	LPDISPATCH result;
	InvokeHelper(0x60020001, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

LPDISPATCH IAPPasteBuffer::GetConflicts()
{
	LPDISPATCH result;
	InvokeHelper(0x60020002, DISPATCH_PROPERTYGET, VT_DISPATCH, (void*)&result, NULL);
	return result;
}

void IAPPasteBuffer::SetContext(const VARIANT& newValue)
{
	static BYTE parms[] =
		VTS_VARIANT;
	InvokeHelper(0x60020003, DISPATCH_PROPERTYPUT, VT_EMPTY, NULL, parms,
		 &newValue);
}

void IAPPasteBuffer::Import(LPCTSTR filename)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020004, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename);
}

void IAPPasteBuffer::Export(LPCTSTR filename)
{
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020005, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 filename);
}

void IAPPasteBuffer::Commit(const VARIANT& force, const VARIANT& errorLog)
{
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020006, DISPATCH_METHOD, VT_EMPTY, NULL, parms,
		 &force, &errorLog);
}


/////////////////////////////////////////////////////////////////////////////
// IHNodeSelection properties

/////////////////////////////////////////////////////////////////////////////
// IHNodeSelection operations

LPDISPATCH IHNodeSelection::Select(LPCTSTR nodePath)
{
	LPDISPATCH result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020000, DISPATCH_METHOD, VT_DISPATCH, (void*)&result, parms,
		nodePath);
	return result;
}

VARIANT IHNodeSelection::SelectNodes(LPCTSTR nodePath)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020001, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		nodePath);
	return result;
}

VARIANT IHNodeSelection::SelectValues(LPCTSTR nodePath)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_BSTR;
	InvokeHelper(0x60020002, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		nodePath);
	return result;
}

VARIANT IHNodeSelection::SelectAttributes(LPCTSTR nodePath, const VARIANT& attributes)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_BSTR VTS_VARIANT;
	InvokeHelper(0x60020003, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		nodePath, &attributes);
	return result;
}

VARIANT IHNodeSelection::SelectLabelAttributes(LPCTSTR nodePath, long Dimension, const VARIANT& attributes)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_BSTR VTS_I4 VTS_VARIANT;
	InvokeHelper(0x60020004, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		nodePath, Dimension, &attributes);
	return result;
}

VARIANT IHNodeSelection::SelectSingleNodeAttributes(LPCTSTR nodePath, const VARIANT& attributes)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_BSTR VTS_VARIANT;
	InvokeHelper(0x60020005, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		nodePath, &attributes);
	return result;
}

VARIANT IHNodeSelection::SelectAttributesOnNodes(const VARIANT& nodePathArray, const VARIANT& attributes)
{
	VARIANT result;
	static BYTE parms[] =
		VTS_VARIANT VTS_VARIANT;
	InvokeHelper(0x60020006, DISPATCH_METHOD, VT_VARIANT, (void*)&result, parms,
		&nodePathArray, &attributes);
	return result;
}
