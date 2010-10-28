// mfctestdlgView.cpp : implementation of the CmfctestdlgView class
//

#include "stdafx.h"
#include "mfctestdlg.h"

#include "mfctestdlgDoc.h"
#include "mfctestdlgView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CmfctestdlgView

IMPLEMENT_DYNCREATE(CmfctestdlgView, CView)

BEGIN_MESSAGE_MAP(CmfctestdlgView, CView)
	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, &CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, &CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, &CView::OnFilePrintPreview)
END_MESSAGE_MAP()

// CmfctestdlgView construction/destruction

CmfctestdlgView::CmfctestdlgView()
{
	// TODO: add construction code here

}

CmfctestdlgView::~CmfctestdlgView()
{
}

BOOL CmfctestdlgView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CView::PreCreateWindow(cs);
}

// CmfctestdlgView drawing

void CmfctestdlgView::OnDraw(CDC* /*pDC*/)
{
	CmfctestdlgDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	if (!pDoc)
		return;

	// TODO: add draw code for native data here
}


// CmfctestdlgView printing

BOOL CmfctestdlgView::OnPreparePrinting(CPrintInfo* pInfo)
{
	// default preparation
	return DoPreparePrinting(pInfo);
}

void CmfctestdlgView::OnBeginPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add extra initialization before printing
}

void CmfctestdlgView::OnEndPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add cleanup after printing
}


// CmfctestdlgView diagnostics

#ifdef _DEBUG
void CmfctestdlgView::AssertValid() const
{
	CView::AssertValid();
}

void CmfctestdlgView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CmfctestdlgDoc* CmfctestdlgView::GetDocument() const // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CmfctestdlgDoc)));
	return (CmfctestdlgDoc*)m_pDocument;
}
#endif //_DEBUG


// CmfctestdlgView message handlers
