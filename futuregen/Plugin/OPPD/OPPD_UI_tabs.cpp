#include "OPPD_UI_tabs.h"





OPPD_UI_tabs::OPPD_UI_tabs(wxWindow* parent, wxWindowID id,
            const wxPoint& pos ,
            const wxSize& size ,
            long style )
:wxNotebook(parent, id, pos, size, style)
{
	_entriesPage = 0;

}


OPPD_UI_tabs::~OPPD_UI_tabs()
{
	//delete _entriesPage;
}

void OPPD_UI_tabs::createTabPages()
{
	_entriesPage = new OPPD_UI_entries(this);
	AddPage( _entriesPage, _T("Inputs"), true);

	_outputsPage = new OPPD_UI_outputs(this);
	AddPage( _outputsPage, _T("Outputs"), false);

}
