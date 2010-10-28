// mfctestdlgDoc.h : interface of the CmfctestdlgDoc class
//


#pragma once


class CmfctestdlgDoc : public CDocument
{
protected: // create from serialization only
	CmfctestdlgDoc();
	DECLARE_DYNCREATE(CmfctestdlgDoc)

// Attributes
public:

// Operations
public:

// Overrides
public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);

// Implementation
public:
	virtual ~CmfctestdlgDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	DECLARE_MESSAGE_MAP()
};


