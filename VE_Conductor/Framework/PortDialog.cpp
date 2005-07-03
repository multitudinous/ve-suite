#include "VE_Conductor/Framework/PortDialog.h"
#include <iostream>

BEGIN_EVENT_TABLE(PortDialog, wxDialog)
  EVT_BUTTON(wxID_OK, PortDialog::OnOK)
END_EVENT_TABLE()

PortDialog::PortDialog(const wxString& title)
    : wxDialog((wxWindow *) NULL, -1, title, wxDefaultPosition, wxDefaultSize)
{
  wxSize sz1, sz2, syn;

  sz1.Set(120, 17);
  sz2.Set(250, 300);
  wxBoxSizer *top_sizer = new wxBoxSizer(wxVERTICAL);

  wxBoxSizer *first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *third_row = new wxBoxSizer(wxHORIZONTAL);

  syngas = new ListTable(this, -1, wxDefaultPosition, 
			 sz2);

  ok = new wxButton(this, wxID_OK, "OK");
  top_sizer->Add(first_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(second_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(third_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(syngas, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(ok, 0, wxALIGN_CENTER_HORIZONTAL);

  first_row->Add(new wxStaticText(this, -1, "Temperature (F): ", wxDefaultPosition, sz1), 1, wxALIGN_LEFT);
  temp = new wxTextCtrl(this, -1, "", wxDefaultPosition, sz1, wxTE_READONLY);
  first_row->Add(temp, 0, wxALIGN_CENTER_HORIZONTAL);
  
  second_row->Add(new wxStaticText(this, -1, "Pressure (psi): ", wxDefaultPosition, sz1), 1, wxALIGN_LEFT);
  pres = new wxTextCtrl(this, -1, "", wxDefaultPosition, sz1, wxTE_READONLY);
  second_row->Add(pres, 0, wxALIGN_CENTER_HORIZONTAL);
  
  third_row->Add(new wxStaticText(this, -1, "Flow rate (ton/hr): ", wxDefaultPosition, sz1), 1, wxALIGN_LEFT);
  flrt = new wxTextCtrl(this, -1, "", wxDefaultPosition, sz1, wxTE_READONLY);
  third_row->Add(flrt, 0, wxALIGN_CENTER_HORIZONTAL);

  SetSizer( top_sizer );
  //SetAutoLayout(TRUE);
  top_sizer->Fit(this);
  
}


void PortDialog::Set3Cols(const std::vector<wxString>& col1, const std::vector<wxString>& col2, const std::vector<wxString>& col3)
{
  int len = col1.size();
  int i;
  
  std::vector<wxString> row;

  row.resize(3);
  syngas->ClearAll();
  for (i=0; i<len; i++)
    {
      row[0]=col1[i];
      row[1]=col2[i];
      row[2]=col3[i];
      syngas->AddRow(row);
    }
}

PortDialog::~PortDialog()
{
}

void PortDialog::SetVal(const wxString &var, const wxString &val)
{
  if (var=="TEMP")
    temp->SetValue(val);
  else if (var=="PRES")
    pres->SetValue(val);
  else if (var=="FLRT")
    flrt->SetValue(val);
}
