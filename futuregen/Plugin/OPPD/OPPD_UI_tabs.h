#ifndef _OPPD_UI_TABS_H_
#define _OPPD_UI_TABS_H_

#include "OPPD_UI_entries.h"
#include "OPPD_UI_outputs.h"
#include "OPPD_UI_constants.h"


class OPPD_UI_entries;
class OPPD_UI_outputs;
class OPPD_UI_constants;

class OPPD_UI_tabs: public wxNotebook{
public:

   OPPD_UI_tabs(wxWindow* parent, wxWindowID =-1,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, 
            long style = 0);

   ~OPPD_UI_tabs();

   void createTabPages();

   OPPD_UI_entries* _entriesPage;
   OPPD_UI_outputs* _outputsPage;
   OPPD_UI_constants* _constantsPage;



};
#endif //_OPPD_UI_TABS_H_