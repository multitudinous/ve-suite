// DlgProxy.h: header file
//

#pragma once

class CTesterDlg;


// CTesterDlgAutoProxy command target

class CTesterDlgAutoProxy : public CCmdTarget
{
	DECLARE_DYNCREATE(CTesterDlgAutoProxy)

	CTesterDlgAutoProxy();           // protected constructor used by dynamic creation

// Attributes
public:
	CTesterDlg* m_pDialog;

// Operations
public:

// Overrides
	public:
	virtual void OnFinalRelease();

// Implementation
protected:
	virtual ~CTesterDlgAutoProxy();

	// Generated message map functions

	DECLARE_MESSAGE_MAP()
	DECLARE_OLECREATE(CTesterDlgAutoProxy)

	// Generated OLE dispatch map functions

	DECLARE_DISPATCH_MAP()
	DECLARE_INTERFACE_MAP()
};

