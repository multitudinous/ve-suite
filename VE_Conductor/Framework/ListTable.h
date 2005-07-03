// ListTable.h: interface for the ListTable class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LISTTABLE_H__745B1AA2_D560_41C1_AC10_748693B1A8EF__INCLUDED_)
#define AFX_LISTTABLE_H__745B1AA2_D560_41C1_AC10_748693B1A8EF__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <wx/wx.h>
#include <wx/listctrl.h>
#include <vector>

class ListTable : public wxListCtrl  
{
public:
	ListTable(wxWindow *parent=NULL,
               const wxWindowID id=-1,
               const wxPoint& pos=wxDefaultPosition,
               const wxSize& size=wxDefaultSize);
	virtual ~ListTable();

	void SetTitle(std::vector<wxString> titles);
	void AddRow(std::vector<wxString> vals);
	void DelRow(int i);

protected:
	wxListItemAttr m_attr;
	int num_of_row;

};

#endif // !defined(AFX_LISTTABLE_H__745B1AA2_D560_41C1_AC10_748693B1A8EF__INCLUDED_)
