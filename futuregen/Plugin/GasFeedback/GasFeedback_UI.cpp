#include "GasFeedback_UI.h"
BEGIN_EVENT_TABLE(GasFeedback_UI_Dialog, UIDialog)
  EVT_BUTTON(ADD, GasFeedback_UI_Dialog::OnAdd)
  EVT_BUTTON(DEL, GasFeedback_UI_Dialog::OnDel)
  EVT_LISTBOX_DCLICK(SPEC_LIST, GasFeedback_UI_Dialog::OnEditSpec)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(GasFeedback_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
GasFeedback_UI_Dialog
::GasFeedback_UI_Dialog
(wxWindow* parent, int id,
  long* iterations,
  vector<string>* species,
  vector<string>* sel_species,
  vector<string>* max_error)
: UIDialog((wxWindow *) parent, id, "GasFeedback"),
  p_iterations(iterations),
  p_species(species),
  p_sel_species(sel_species),
  p_max_error(max_error)
{
  wxBoxSizer* toptop= new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);

  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT); 
  
  wxBoxSizer *data1_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data2_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data3_row = new wxBoxSizer(wxHORIZONTAL);
  
  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data1_row, 0);//, wxALIGN_CENTER_HORIZONTAL); 
  top_sizer->Add(10, 5, 0); 
  top_sizer->Add(data2_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(data3_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 5, 0);
  
  wxStaticText * label1 = new wxStaticText(this, -1, " Max. Iterations:", wxDefaultPosition, wxSize(200, 17));
  t_iterations = new wxTextCtrl(this, -1, wxT("1"), wxDefaultPosition, wxSize(160, 20));
  data1_row->Add(label1);
  data1_row->Add(t_iterations);

  wxBoxSizer* spec_row = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* sel_row = new wxBoxSizer(wxVERTICAL);

  data2_row->Add(spec_row);
  data2_row->Add(sel_row);
  
  lb_species = new wxListBox(this, -1, wxDefaultPosition, wxSize(120, 500), 0, NULL, wxLB_MULTIPLE|wxLB_SORT|wxLB_NEEDED_SB);
  spec_row->Add(new wxStaticText(this, -1, "    Available", wxDefaultPosition, wxSize(120, 17)));
  spec_row->Add(lb_species);

  wxBoxSizer* title_row = new wxBoxSizer(wxHORIZONTAL);
  sel_row->Add(title_row);

  title_row->Add(new wxStaticText(this, -1, "Variable", wxDefaultPosition, wxSize(100, 17)));
  title_row->Add(new wxStaticText(this, -1, "Threshold", wxDefaultPosition, wxSize(120, 17)));

  lb_selspec_thresh = new wxListBox(this, SPEC_LIST, wxDefaultPosition, wxSize(240,500), 0, NULL, wxLB_SINGLE|wxLB_SORT);  
  sel_row->Add(lb_selspec_thresh);

  b_add = new wxButton(this, ADD, "Add");
  b_del = new wxButton(this, DEL, "Del");

  data3_row->Add(b_add, 0, wxALIGN_CENTER_HORIZONTAL);
  data3_row->Add(b_del, 0, wxALIGN_CENTER_HORIZONTAL);
  data3_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  data3_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);

  SetSizer(toptop);
  toptop->Layout();
  SetAutoLayout(TRUE);
  toptop->Fit(this);
}

/////////////////////////////////////////////////////
GasFeedback_UI_Dialog
::~GasFeedback_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool GasFeedback_UI_Dialog::TransferDataFromWindow()
{
  wxString txt;

  txt = t_iterations->GetValue();
  (*p_iterations) = atoi(txt.c_str());

  int num;
  int i;
  wxString val, var;
  
  num = lb_selspec_thresh->GetCount();
  p_sel_species->clear();
  p_max_error->clear();

  for (i=0; i<num; i++)
    {
      txt = lb_selspec_thresh->GetString(i);
      val = ToValue(txt, var);
      p_sel_species->push_back(var.c_str());
      p_max_error->push_back(val.c_str());
    }

  return true;
}

////////////////////////////////////////////////////
bool GasFeedback_UI_Dialog::TransferDataToWindow()
{
  wxString txt1;
  
  txt1<<(*p_iterations);
  t_iterations->SetValue(txt1);

  //Here should include the code to read in all the availables
  int i;
  int num;
  wxString var, val;

  lb_species->Clear();
  num = p_species->size();
  for (i=0; i<num; i++)
    lb_species->Append((*p_species)[i].c_str());
  
  lb_selspec_thresh->Clear();
  num = p_sel_species->size();
  for (i=0; i<num; i++)
    {
      var=(*p_sel_species)[i].c_str();
      val=(*p_max_error)[i].c_str();
      lb_selspec_thresh->Append(ToString(var, val));
    }
  
  
  return true;
}

void GasFeedback_UI_Dialog::Lock(bool l)
{
}

void GasFeedback_UI_Dialog::OnAdd(wxCommandEvent& event)
{
  wxArrayInt selections;
  int i;
  map<wxString, wxString> items;
  map<wxString, wxString>::iterator iter;

  wxString cur_val;
  wxString cur_var;
  int num_sels, count;

  items.clear();
  
  count = lb_selspec_thresh->GetCount();
  for (i=0; i<count; i++)
    {
      cur_val = ToValue(lb_selspec_thresh->GetString(i), cur_var);
      items.insert(pair<wxString, wxString>(cur_var, cur_val));
    }

  num_sels = lb_species->GetSelections(selections);
  for (i=0; i<num_sels; i++)
    items.insert(pair<wxString, wxString>(lb_species->GetString(selections[i]), _T("0.0")));
  
  lb_selspec_thresh->Clear();

  for (iter = items.begin(); iter!=items.end(); iter++)
    lb_selspec_thresh->Append(ToString(iter->first, iter->second));
}

void GasFeedback_UI_Dialog::OnDel(wxCommandEvent& event)
{
  wxArrayInt selections;
  int num_sels, i;
  
  num_sels = lb_selspec_thresh->GetSelections(selections);
  
  for (i=0; i<num_sels; i++)
    lb_selspec_thresh->Delete(selections[i]-i);
  
}

void GasFeedback_UI_Dialog::OnEditSpec(wxCommandEvent& event)
{
  wxString var;
  wxString val;
  wxString result;
  int sel;
  EditDialog2* e_dlg = new EditDialog2(this, -1);

  val = ToValue(lb_selspec_thresh->GetStringSelection(), var);

  e_dlg->m_var->SetLabel(var);
  e_dlg->m_val->SetValue(val);

  e_dlg->ShowModal();

  val = e_dlg->m_val->GetValue();
  
  result = ToString(var, val);
  sel = lb_selspec_thresh->GetSelection();
  lb_selspec_thresh->SetString(sel, result);
  e_dlg->Close(); 
}

wxString GasFeedback_UI_Dialog::ToValue(const wxString inp, wxString &outp)
{
  char var[80];
  char result[80];
  
  sscanf(inp.c_str(), "%s    %s", var, result);
  outp = _T(var);
  return _T(result);
}

wxString GasFeedback_UI_Dialog::ToString(const wxString inp, const wxString val)
{
  char result[1024];

  sprintf(result, "%-16s     %s", inp.c_str(), val.c_str());
  return _T(result);
}

EditDialog2::EditDialog2(wxWindow *parent, wxWindowID id)
  :wxDialog(parent, id, _T("Edit"))
{
  wxBoxSizer* toptop= new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);

  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  wxBoxSizer* data_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data_row, 0); 
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin
  
  m_var = new wxStaticText(this, -1, " Var name ", wxDefaultPosition, wxSize(80, 17));
  m_val = new wxTextCtrl(this, -1, wxT("15.70"), wxDefaultPosition, wxSize(80, 20));
  data_row->Add(m_var, 0, wxALIGN_CENTER_HORIZONTAL);
  data_row->Add(m_val, 0, wxALIGN_CENTER_HORIZONTAL);

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  toptop->Fit(this);
  SetAutoLayout(true);
  SetSizer(toptop);
  
}
