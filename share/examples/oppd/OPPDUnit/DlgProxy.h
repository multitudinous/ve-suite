// DlgProxy.h: header file
//

#pragma once

class COPPDUnitDlg;


// COPPDUnitDlgAutoProxy command target

class COPPDUnitDlgAutoProxy : public CCmdTarget
{
	DECLARE_DYNCREATE(COPPDUnitDlgAutoProxy)

	COPPDUnitDlgAutoProxy();           // protected constructor used by dynamic creation

// Attributes
public:
	COPPDUnitDlg* m_pDialog;

// Operations
public:

// Overrides
	public:
	virtual void OnFinalRelease();

// Implementation
protected:
	virtual ~COPPDUnitDlgAutoProxy();

	// Generated message map functions

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(COPPDUnitDlgAutoProxy)

	// Generated OLE dispatch map functions

	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()
};

