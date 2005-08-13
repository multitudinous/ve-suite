#include "VE_Conductor/Framework/TextResultDialog.h"
#include "VE_Conductor/Framework/TexTable.h"

#include <iostream>

BEGIN_EVENT_TABLE(TextResultDialog, wxDialog)
  EVT_BUTTON(wxID_OK, TextResultDialog::OnOK)
END_EVENT_TABLE()

TextResultDialog::TextResultDialog(wxWindow * parent, const wxString& title, wxSize tabsize)
    : UIDialog((wxWindow *)parent, -1, title)
{
  wxSize syn;
  wxBoxSizer* toptop= new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);

  
  //sz2.Set(250, 300);

  left_margin->Add(10, 10);
  right_margin->Add(10, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  syngas = new TexTable(this, -1, wxDefaultPosition, 
			 tabsize);

  ok = new wxButton(this, wxID_OK, "OK");
  top_sizer->Add(10, 10, 0);
  top_sizer->Add(new wxStaticText(this, -1, "Summary Data   ", wxDefaultPosition, wxDefaultSize), 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(syngas, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);
  
}


void TextResultDialog::Set2Cols(const std::vector<wxString>& col1, const std::vector<wxString>& col2)
{
  int len = col1.size();
  int i;
  
  std::vector<wxString> row;

  row.resize(2);
  //  syngas->Clear();
  for (i=0; i<len; i++)
    {
      row[0]=col1[i];
      row[1]=col2[i];
      
      syngas->AddRow(row);
    }
}

TextResultDialog::~TextResultDialog()
{
}

