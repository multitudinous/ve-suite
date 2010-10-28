// mfctestdlgView.h : interface of the CmfctestdlgView class
//


#pragma once


class CmfctestdlgView : public CView
{
protected: // create from serialization only
	CmfctestdlgView();
	DECLARE_DYNCREATE(CmfctestdlgView)

// Attributes
public:
	CmfctestdlgDoc* GetDocument() const;

// Operations
public:

// Overrides
public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
protected:
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);

// Implementation
public:
	virtual ~CmfctestdlgView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in mfctestdlgView.cpp
inline CmfctestdlgDoc* CmfctestdlgView::GetDocument() const
   { return reinterpret_cast<CmfctestdlgDoc*>(m_pDocument); }
#endif

