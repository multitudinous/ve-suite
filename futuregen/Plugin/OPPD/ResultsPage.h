#ifndef _RESULTS_PAGE_H
#define _RESULTS_PAGE_H
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>
#include <wx/notebook.h>
#include "EntryPage.h"
#include "ResultsPage.h"

enum {

};

class ResultsPage: public wxPanel{
public:
	ResultsPage(wxNotebook* tControl);

	DECLARE_EVENT_TABLE()
};
#endif //_RESULTS_PAGE_H_