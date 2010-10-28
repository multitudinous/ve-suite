// mfctestdlgDoc.cpp : implementation of the CmfctestdlgDoc class
//

#include "stdafx.h"
#include "mfctestdlg.h"

#include "mfctestdlgDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CmfctestdlgDoc

IMPLEMENT_DYNCREATE(CmfctestdlgDoc, CDocument)

BEGIN_MESSAGE_MAP(CmfctestdlgDoc, CDocument)
END_MESSAGE_MAP()


// CmfctestdlgDoc construction/destruction

CmfctestdlgDoc::CmfctestdlgDoc()
{
	// TODO: add one-time construction code here

}

CmfctestdlgDoc::~CmfctestdlgDoc()
{
}

BOOL CmfctestdlgDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}




// CmfctestdlgDoc serialization

void CmfctestdlgDoc::Serialize(CArchive& ar)
{
	if (ar.IsStoring())
	{
		// TODO: add storing code here
	}
	else
	{
		// TODO: add loading code here
	}
}


// CmfctestdlgDoc diagnostics

#ifdef _DEBUG
void CmfctestdlgDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CmfctestdlgDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG


// CmfctestdlgDoc commands
