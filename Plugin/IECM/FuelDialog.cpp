#include "FuelDialog.h"
#include "wx/combobox.h"
#include "interface.h"
#include <math.h>


BEGIN_EVENT_TABLE(FuelDialog, UIDialog)
EVT_COMBOBOX(COMBO_COAL, FuelDialog::OnChooseCoal)
EVT_BUTTON(COAL_SAVE_B, FuelDialog::OnSaveCoal)
EVT_BUTTON(ULTI_NORM_B, FuelDialog::OnNormUlti)
EVT_BUTTON(PROX_CALC_B, FuelDialog::OnCalcProx)
EVT_BUTTON(ASHC_NORM_B, FuelDialog::OnNormAsh)
EVT_TEXT(COAL_COST, FuelDialog::ChangeCost2)
EVT_TEXT(HHV, FuelDialog::ChangeCost2)
END_EVENT_TABLE()


////////////////////////////////////////////////////////////////////////////
FuelDialog::FuelDialog(wxWindow *parent, int id ,
                       string *coal_name_s,
                       double *ulti_c_d,
                       double *ulti_h_d,
                       double *ulti_o_d,
                       double *ulti_n_d,
                       double *ulti_s_d,
                       double *ulti_cl_d,
                       double *ulti_ash_d,
                       double *prox_h2o_d,
                       double *prox_vm_d,
                       double *prox_ash_d,
                       double *prox_fc_d,
                       double *ashc_sio2_d,
                       double *ashc_al2o3_d,
                       double *ashc_tio2_d,
                       double *ashc_fe2o3_d,
                       double *ashc_cao_d,
                       double *ashc_mgo_d,
                       double *ashc_na2o_d,
                       double *ashc_k2o_d,
                       double *ashc_so3_d,
                       double *ashc_p2o5_d,
                       double *ashc_bao_d,
                       double *ashc_sro_d,
                       double *hhv_d,
                       double *coal_cost_d
                       )
                       :UIDialog((wxWindow *) parent, id, "Fuel Specification"),
                       coal_name_s_(coal_name_s),
                       ulti_c_d_(ulti_c_d),
                       ulti_h_d_(ulti_h_d),
                       ulti_o_d_(ulti_o_d),
                       ulti_n_d_(ulti_n_d),
                       ulti_s_d_(ulti_s_d),
                       ulti_cl_d_(ulti_cl_d),
                       ulti_ash_d_(ulti_ash_d),
                       prox_h2o_d_(prox_h2o_d),
                       prox_vm_d_(prox_vm_d),
                       prox_ash_d_(prox_ash_d),
                       prox_fc_d_(prox_fc_d),
                       ashc_sio2_d_(ashc_sio2_d),
                       ashc_al2o3_d_(ashc_al2o3_d),
                       ashc_tio2_d_(ashc_tio2_d),
                       ashc_fe2o3_d_(ashc_fe2o3_d),
                       ashc_cao_d_(ashc_cao_d),
                       ashc_mgo_d_(ashc_mgo_d),
                       ashc_na2o_d_(ashc_na2o_d),
                       ashc_k2o_d_(ashc_k2o_d),
                       ashc_so3_d_(ashc_so3_d),
                       ashc_p2o5_d_(ashc_p2o5_d),
                       ashc_bao_d_(ashc_bao_d),
                       ashc_sro_d_(ashc_sro_d),
                       hhv_d_(hhv_d),
                       coal_cost_d_(coal_cost_d),
                       hhv(NULL),
                       coal_cost(NULL),
                       coal_cost2(NULL)
{
    readDatabase("coalData.dat");
    wxSize entry_size;
    wxSize tag_size;
    
    
    tag_size.Set(25, 17);
    
    //Set up the strings in the radio box
    int n_coal , i;
    
    n_coal = coal_name_v.size();
    wxString* coal_type = new wxString[n_coal];
    
    for (i=0; i<n_coal; i++)
        coal_type[i]=coal_name_v[i].c_str();
    
    
    //Set up the sizer so we can lay things well
    //The this will be four rows from top to bottom
    wxBoxSizer* toptop = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);
    left_margin->Add(5, 10);
    right_margin->Add(5, 10);
    
    
    toptop->Add(left_margin, 0, wxALIGN_LEFT);
    toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
    toptop->Add(right_margin, 0, wxALIGN_RIGHT);
    
    
    
    wxBoxSizer *first_row = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *second_row = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *third_row = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *forth_row = new wxBoxSizer(wxHORIZONTAL);
    
    
    top_sizer->Add(10, 5);
    top_sizer->Add(first_row, 0, wxALIGN_LEFT); //
    top_sizer->Add(10, 5);
    top_sizer->Add(second_row, 0, wxALIGN_LEFT);
    top_sizer->Add(10, 5);
    top_sizer->Add(third_row, 0, wxALIGN_CENTER_HORIZONTAL);
    top_sizer->Add(10, 12);
    top_sizer->Add(forth_row, 0, wxALIGN_CENTER_HORIZONTAL);
    
    
    //Instantiate the first row
    combo_coal = new wxComboBox(this, COMBO_COAL, coal_name_s_->c_str(), wxDefaultPosition, wxDefaultSize, n_coal, coal_type, wxCB_DROPDOWN|wxCB_READONLY);
    
    first_row->Add(5,5);
    first_row->Add(new wxStaticText(this, -1, wxT("Reset Coal:"), wxDefaultPosition, wxSize(95, 17)), 0, wxALIGN_CENTER_HORIZONTAL);
    
    
    first_row->Add(combo_coal, 0,  wxALIGN_CENTER_HORIZONTAL);
    
    
    //The second row
    coal_name = new wxTextCtrl(this, COAL_NAME, wxT("        "), wxDefaultPosition, wxSize(180, 20));
    coal_name->Disable();
    coal_save_b = new wxButton(this, COAL_SAVE_B, wxT("Save"));
    coal_save_b->Disable();
    
    
    second_row->Add(5,5);
    second_row->Add(new wxStaticText(this, -1, wxT("Edit/New Coal: "), wxDefaultPosition, wxSize(95, 17)));
    second_row->Add(coal_name, 0, wxALIGN_CENTER_HORIZONTAL);
    second_row->Add(5, 10);
    second_row->Add(coal_save_b, 0, wxALIGN_RIGHT);
    
    //The third row
    wxBoxSizer *third_left = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer *third_right = new wxBoxSizer(wxVERTICAL);
    third_row->Add(third_left, 0, wxALIGN_LEFT);
    third_row->Add(5, 10);
    third_row->Add(third_right, 0, wxALIGN_RIGHT);
    
    //The third left this
    wxBoxSizer *third_left_up = new wxBoxSizer(wxHORIZONTAL);
    wxStaticBox *heating_value = new wxStaticBox(this, -1, "Heating Value");
    wxStaticBoxSizer *third_left_middle = new wxStaticBoxSizer(heating_value, wxHORIZONTAL);
    wxStaticBox *cost_value = new wxStaticBox(this, -1, "Fuel Cost");
    wxStaticBoxSizer *third_left_down = new wxStaticBoxSizer(cost_value, wxVERTICAL);
    
    
    
    third_left->Add(third_left_up, 0, wxALIGN_TOP);
    third_left->Add(10, 5);
    third_left->Add(third_left_middle, 0, wxALIGN_CENTER_VERTICAL);
    third_left->Add(10, 5);
    third_left->Add(third_left_down);
    
    
    //The third left up this
    wxStaticBox *ultimate = new wxStaticBox(this, -1, "Ultimate (Wt.%)");
    wxStaticBoxSizer *third_left_up_left = new wxStaticBoxSizer(ultimate, wxVERTICAL);
    wxStaticBox *proximate = new wxStaticBox(this, -1, "Proximate (Wt.%)");
    wxStaticBoxSizer *third_left_up_right = new wxStaticBoxSizer(proximate, wxVERTICAL);
    
    
    third_left_up->Add(third_left_up_left, 0, wxALIGN_LEFT);
    third_left_up->Add(5, 5);
    third_left_up->Add(third_left_up_right, 0, wxALIGN_RIGHT);
    
    
    //The third left up left this
    wxBoxSizer *c_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *h_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *o_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *n_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *s_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *cl_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *ash_s = new wxBoxSizer(wxHORIZONTAL);
    ulti_norm_b = new wxButton(this, ULTI_NORM_B, "Normalize");
    
    
    third_left_up_left->Add(5,5);
    third_left_up_left->Add(c_s,0);
    third_left_up_left->Add(h_s,0);
    third_left_up_left->Add(o_s,0);
    third_left_up_left->Add(n_s,0);
    third_left_up_left->Add(s_s,0);
    third_left_up_left->Add(cl_s,0);
    third_left_up_left->Add(ash_s,0);
    third_left_up_left->Add(5,5);
    third_left_up_left->Add(ulti_norm_b,0, wxALIGN_CENTER_HORIZONTAL);
    
    entry_size.Set(60, 20);
    
    
    ulti_c = new wxTextCtrl(this, ULTI_C, "0.0", wxDefaultPosition, entry_size);
    c_s->Add(new wxStaticText(this, -1, "C  ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    c_s->Add(ulti_c, 0, wxALIGN_RIGHT);
    
    
    ulti_h = new wxTextCtrl(this, ULTI_H, "0.0", wxDefaultPosition, entry_size);
    h_s->Add(new wxStaticText(this, -1, "H  ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    h_s->Add(ulti_h, 0, wxALIGN_RIGHT);
    
    
    ulti_o = new wxTextCtrl(this, ULTI_O, "0.0", wxDefaultPosition, entry_size);
    o_s->Add(new wxStaticText(this, -1, "O  ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    o_s->Add(ulti_o, 0, wxALIGN_RIGHT);
    
    
    ulti_n = new wxTextCtrl(this, ULTI_N, "0.0", wxDefaultPosition, entry_size);
    n_s->Add(new wxStaticText(this, -1, "N  ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    n_s->Add(ulti_n, 0, wxALIGN_RIGHT);
    
    
    ulti_s = new wxTextCtrl(this, ULTI_S, "0.0", wxDefaultPosition, entry_size);
    s_s->Add(new wxStaticText(this, -1, "S  ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    s_s->Add(ulti_s, 0, wxALIGN_RIGHT);
    
    
    ulti_cl = new wxTextCtrl(this, ULTI_CL, "0.0", wxDefaultPosition, entry_size);
    cl_s->Add(new wxStaticText(this, -1, "Cl ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    cl_s->Add(ulti_cl, 0, wxALIGN_RIGHT);
    
    
    ulti_ash = new wxTextCtrl(this, ULTI_ASH, "0.0", wxDefaultPosition, entry_size);
    ash_s->Add(new wxStaticText(this, -1, "Ash", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    ash_s->Add(ulti_ash, 0, wxALIGN_RIGHT);
    
    
    //The third left up right this
    entry_size.Set(85,20);
    
    
    wxBoxSizer *h2o_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *vm_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *pash_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *fc_s = new wxBoxSizer(wxHORIZONTAL);
    prox_calc_b = new wxButton(this, PROX_CALC_B, "Calculate");
    
    
    tag_size.Set(30, 17); 
    
    
    third_left_up_right->Add(5,5);
    third_left_up_right->Add(h2o_s);
    third_left_up_right->Add(vm_s);
    third_left_up_right->Add(pash_s);
    third_left_up_right->Add(fc_s);
    third_left_up_right->Add(5, 5);
    third_left_up_right->Add(prox_calc_b, 0, wxALIGN_CENTER_HORIZONTAL);
    
    
    prox_h2o = new wxTextCtrl(this, PROX_H2O, "0.0", wxDefaultPosition, entry_size);
    h2o_s->Add(new wxStaticText(this, -1, "H2O", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    h2o_s->Add(prox_h2o, 0, wxALIGN_RIGHT);
    
    
    prox_vm = new wxTextCtrl(this, PROX_VM, "0.0", wxDefaultPosition, entry_size);
    vm_s->Add(new wxStaticText(this, -1, "VM ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    vm_s->Add(prox_vm, 0, wxALIGN_RIGHT);
    
    
    prox_ash = new wxTextCtrl(this, PROX_ASH, "0.0", wxDefaultPosition, entry_size);
    pash_s->Add(new wxStaticText(this, -1, "ASH", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    pash_s->Add(prox_ash, 0, wxALIGN_RIGHT);
    prox_ash->Enable(false);
    
    
    prox_fc = new wxTextCtrl(this, PROX_FC, "0.0", wxDefaultPosition, entry_size);
    fc_s->Add(new wxStaticText(this, -1, "FC ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    fc_s->Add(prox_fc, 0, wxALIGN_RIGHT);
    prox_fc->Enable(false);
    
    
    //The third left middle this
    hhv = new wxTextCtrl(this, HHV, "0.0", wxDefaultPosition, entry_size);
    third_left_middle->Add(new wxStaticText(this, -1, "HHV (BTU/lb) ", wxDefaultPosition, wxSize(130, 17)), 0, wxALIGN_LEFT);
    //third_left_middle->Add(5,5);
    third_left_middle->Add(hhv, 0, wxALIGN_RIGHT);
    
    
    //The third left down this
    wxBoxSizer* third_left_down1=new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* third_left_down2=new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* third_left_down3=new wxBoxSizer(wxHORIZONTAL);
    
    
    third_left_down->Add(5,5);
    third_left_down->Add(third_left_down1);
    third_left_down->Add(third_left_down2);
    third_left_down->Add(third_left_down3);
    
    
    coal_cost = new wxTextCtrl(this, COAL_COST, "0.000000");
    third_left_down1->Add(new wxStaticText(this, -1, "Minemouth Cost ($/ton)   ", wxDefaultPosition, wxSize(135, 17)), 0, wxALIGN_LEFT);
    third_left_down1->Add(coal_cost, 0, wxALIGN_RIGHT);
    
    //The third left down2 
    coal_cost3 = new wxTextCtrl(this, COAL_COST3, "0.000000", wxDefaultPosition, wxDefaultSize, wxTE_READONLY);
    third_left_down2->Add(new wxStaticText(this, -1, "Delivered Cost ($/ton) ", wxDefaultPosition, wxSize(135, 17)), 0, wxALIGN_LEFT);
    third_left_down2->Add(coal_cost3, 0, wxALIGN_RIGHT);
    //coal_cost3->Enable(false);
	
    //The third left down2 
    coal_cost2 = new wxTextCtrl(this, COAL_COST2, "0.000000", wxDefaultPosition, wxDefaultSize, wxTE_READONLY);
    third_left_down3->Add(new wxStaticText(this, -1, "Delivered Cost ($/MBtu) ", wxDefaultPosition, wxSize(135, 17)), 0, wxALIGN_LEFT);
    third_left_down3->Add(coal_cost2, 0, wxALIGN_RIGHT);
    //coal_cost2->Enable(false);
    
    //The third right this
    wxStaticBox *ash_compo = new wxStaticBox(this, -1, "Ash Compsition (Wt.%)");
    wxStaticBoxSizer *third_right_up = new wxStaticBoxSizer(ash_compo, wxVERTICAL);
    
    
    third_right->Add(third_right_up);
    
    
    tag_size.Set(42, 17);
    //The third right up this
    wxBoxSizer *sio2_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *al2o3_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *tio2_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *fe2o3_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *cao_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *mgo_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *na2o_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *k2o_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *so3_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *p2o5_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *bao_s = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer *sro_s = new wxBoxSizer(wxHORIZONTAL);
    ashc_norm_b = new wxButton(this, ASHC_NORM_B, "Normalize");
    
    third_right_up->Add(5,5);
    third_right_up->Add(sio2_s);
    third_right_up->Add(al2o3_s);
    third_right_up->Add(tio2_s);
    third_right_up->Add(fe2o3_s);
    third_right_up->Add(cao_s);
    third_right_up->Add(mgo_s);
    third_right_up->Add(na2o_s);
    third_right_up->Add(k2o_s);
    third_right_up->Add(so3_s);
    third_right_up->Add(p2o5_s);
    third_right_up->Add(bao_s);
    third_right_up->Add(sro_s);
    third_right_up->Add(5,22);
    third_right_up->Add(ashc_norm_b, 0, wxALIGN_CENTER_HORIZONTAL);
    
    
    ashc_sio2 = new wxTextCtrl(this, ASHC_SiO2, "0.0", wxDefaultPosition, entry_size);
    sio2_s->Add(new wxStaticText(this, -1, "SiO2 ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    sio2_s->Add(ashc_sio2, 0, wxALIGN_RIGHT);
    
    
    ashc_al2o3 = new wxTextCtrl(this, ASHC_Al2O3, "0.0", wxDefaultPosition, entry_size);
    al2o3_s->Add(new wxStaticText(this, -1, "Al2O3 ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    al2o3_s->Add(ashc_al2o3, 0, wxALIGN_RIGHT);
    
    
    ashc_tio2 = new wxTextCtrl(this, ASHC_TiO2, "0.0", wxDefaultPosition, entry_size);
    tio2_s->Add(new wxStaticText(this, -1, "TiO2 ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    tio2_s->Add(ashc_tio2, 0, wxALIGN_RIGHT);
    
    
    ashc_fe2o3 = new wxTextCtrl(this, ASHC_Fe2O3, "0.0", wxDefaultPosition, entry_size);
    fe2o3_s->Add(new wxStaticText(this, -1, "Fe2O3 ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    fe2o3_s->Add(ashc_fe2o3, 0, wxALIGN_RIGHT);
    
    
    ashc_cao = new wxTextCtrl(this, ASHC_CaO, "0.0", wxDefaultPosition, entry_size);
    cao_s->Add(new wxStaticText(this, -1, "CaO ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    cao_s->Add(ashc_cao, 0, wxALIGN_RIGHT);
    
    
    ashc_mgo = new wxTextCtrl(this, ASHC_MgO, "0.0", wxDefaultPosition, entry_size);
    mgo_s->Add(new wxStaticText(this, -1, "MgO ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    mgo_s->Add(ashc_mgo, 0, wxALIGN_RIGHT);
    
    
    ashc_na2o = new wxTextCtrl(this, ASHC_Na2O, "0.0", wxDefaultPosition, entry_size);
    na2o_s->Add(new wxStaticText(this, -1, "Na2O ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    na2o_s->Add(ashc_na2o, 0, wxALIGN_RIGHT);
    
    
    ashc_k2o = new wxTextCtrl(this, ASHC_K2O, "0.0", wxDefaultPosition, entry_size);
    k2o_s->Add(new wxStaticText(this, -1, "K2O ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    k2o_s->Add(ashc_k2o, 0, wxALIGN_RIGHT);
    
    
    ashc_so3 = new wxTextCtrl(this, ASHC_SO3, "0.0", wxDefaultPosition, entry_size);
    so3_s->Add(new wxStaticText(this, -1, "SO3 ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    so3_s->Add(ashc_so3, 0, wxALIGN_RIGHT);
    
    
    ashc_p2o5 = new wxTextCtrl(this, ASHC_P2O5, "0.0", wxDefaultPosition, entry_size);
    p2o5_s->Add(new wxStaticText(this, -1, "P2O5 ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    p2o5_s->Add(ashc_p2o5, 0, wxALIGN_RIGHT);
    
    
    ashc_bao = new wxTextCtrl(this, ASHC_BaO, "0.0", wxDefaultPosition, entry_size);
    bao_s->Add(new wxStaticText(this, -1, "BaO ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    bao_s->Add(ashc_bao, 0, wxALIGN_RIGHT);
    
    
    ashc_sro = new wxTextCtrl(this, ASHC_SrO, "0.0", wxDefaultPosition, entry_size);
    sro_s->Add(new wxStaticText(this, -1, "SrO ", wxDefaultPosition, tag_size), 0, wxALIGN_LEFT);
    sro_s->Add(ashc_sro, 0, wxALIGN_RIGHT);
    
    //The forth row
    ok_b = new wxButton(this, wxID_OK, "OK");
    //  cancel_b = new wxButton(this, wxID_CANCEL, "Cancel");
    
    
    forth_row->Add(ok_b, 0, wxALIGN_CENTER_HORIZONTAL);
    //  forth_row->Add(cancel_b, 0, wxALIGN_CENTER_HORIZONTAL);
    
    
    this->SetAutoLayout( true );
    this->SetSizer(toptop); 
    
    toptop->Fit(this);
}

//////////////////////////////////////////////////////////////////////////////
bool FuelDialog::TransferDataToWindow()
{
    //wxString txt1;
    //txt1<<(*coal_cost_d_);
    //wxMessageBox(txt1);
    
    notready = true;
    coal_name->SetValue(coal_name_s_->c_str());
    double2entry(coal_cost, coal_cost_d_);
    double2entry(ulti_c, ulti_c_d_);
    double2entry(ulti_h, ulti_h_d_);
    double2entry(ulti_o, ulti_o_d_);
    double2entry(ulti_n, ulti_n_d_);
    double2entry(ulti_s, ulti_s_d_);
    double2entry(ulti_cl, ulti_cl_d_);
    double2entry(ulti_ash, ulti_ash_d_);
    double2entry(prox_h2o, prox_h2o_d_);
    double2entry(prox_vm, prox_vm_d_);
    double2entry(prox_ash, prox_ash_d_);
    double2entry(prox_fc, prox_fc_d_);
    double2entry(ashc_sio2, ashc_sio2_d_);
    double2entry(ashc_al2o3, ashc_al2o3_d_);
    double2entry(ashc_tio2, ashc_tio2_d_);
    double2entry(ashc_fe2o3, ashc_fe2o3_d_);
    double2entry(ashc_cao, ashc_cao_d_);
    double2entry(ashc_mgo, ashc_mgo_d_);
    double2entry(ashc_na2o, ashc_na2o_d_);
    double2entry(ashc_k2o, ashc_k2o_d_);
    double2entry(ashc_so3, ashc_so3_d_);
    double2entry(ashc_p2o5, ashc_p2o5_d_);
    double2entry(ashc_bao, ashc_bao_d_);
    double2entry(ashc_sro, ashc_sro_d_);
    double2entry(hhv, hhv_d_);
    notready = false;
    
    
    wxCommandEvent event;
    
    ChangeCost2(event);  
    return true;
    
    
}

/////////////////////////////////////////////////////////////////
bool FuelDialog::TransferDataFromWindow()
{
    int result;
    double total;
    wxCommandEvent event;
    char buffer[1024];
    
    *coal_name_s_ = (coal_name->GetValue()).c_str();
    entry2double(ulti_c, ulti_c_d_);
    entry2double(ulti_h, ulti_h_d_);
    entry2double(ulti_o, ulti_o_d_);
    entry2double(ulti_n, ulti_n_d_);
    entry2double(ulti_s, ulti_s_d_);
    entry2double(ulti_cl, ulti_cl_d_);
    entry2double(ulti_ash, ulti_ash_d_);
    entry2double(prox_h2o, prox_h2o_d_);
    entry2double(prox_vm, prox_vm_d_);
    entry2double(prox_ash, prox_ash_d_);
    entry2double(prox_fc, prox_fc_d_);
    entry2double(ashc_sio2, ashc_sio2_d_);
    entry2double(ashc_al2o3, ashc_al2o3_d_);
    entry2double(ashc_tio2, ashc_tio2_d_);
    entry2double(ashc_fe2o3, ashc_fe2o3_d_);
    entry2double(ashc_cao, ashc_cao_d_);
    entry2double(ashc_mgo, ashc_mgo_d_);
    entry2double(ashc_na2o, ashc_na2o_d_);
    entry2double(ashc_k2o, ashc_k2o_d_);
    entry2double(ashc_so3, ashc_so3_d_);
    entry2double(ashc_p2o5, ashc_p2o5_d_);
    entry2double(ashc_bao, ashc_bao_d_);
    entry2double(ashc_sro, ashc_sro_d_);
    entry2double(hhv, hhv_d_);
    entry2double(coal_cost, coal_cost_d_);
    
    
    total = *ashc_sio2_d_+*ashc_al2o3_d_+*ashc_tio2_d_+*ashc_fe2o3_d_
        +*ashc_cao_d_+*ashc_mgo_d_+*ashc_na2o_d_+*ashc_k2o_d_+*ashc_so3_d_
        +*ashc_p2o5_d_+*ashc_bao_d_+*ashc_sro_d_;
    
    
    sprintf(buffer, "Ash total is %lf\n", total);
    if (fabs(total-100.0)>1E-5)
    {
        // wxMessageBox(buffer);
        result = wxMessageBox("Ash composition not add up to 100 percent. Normalize it?", "Warning!", wxYES_NO);
        if (result == wxYES)
        {
            *ashc_sio2_d_ = *ashc_sio2_d_ / total *100.0;
            *ashc_al2o3_d_ = *ashc_al2o3_d_ / total *100.0;
            *ashc_tio2_d_ = *ashc_tio2_d_ / total *100.0;
            *ashc_fe2o3_d_ = *ashc_fe2o3_d_ / total *100.0;
            *ashc_cao_d_ = *ashc_cao_d_ / total *100.0;
            *ashc_mgo_d_ = *ashc_mgo_d_ / total *100.0;
            *ashc_na2o_d_ = *ashc_na2o_d_ / total *100.0;
            *ashc_k2o_d_ = *ashc_k2o_d_ / total *100.0;
            *ashc_so3_d_ = *ashc_so3_d_ / total *100.0;
            *ashc_p2o5_d_ = *ashc_p2o5_d_ / total *100.0;
            *ashc_bao_d_ = *ashc_bao_d_ / total *100.0;
            *ashc_sro_d_ = *ashc_sro_d_ / total *100.0;
        }
    }
    
    
    total =  *ulti_c_d_ + *ulti_h_d_ + *ulti_o_d_
        + *ulti_n_d_ + *ulti_s_d_ + *ulti_cl_d_ + *ulti_ash_d_;
    
    sprintf(buffer, "ulti total is %lf\n", total);
    if (fabs(total-100.0)>1E-5)
    {
        //wxMessageBox(buffer);
        result = wxMessageBox("Coal composition not add up to 100 percent. Normalize it?", "Warning!", wxYES_NO);
        if (result == wxYES)
        {
            *ulti_c_d_ = *ulti_c_d_ / total *100.0;
            *ulti_h_d_ = *ulti_h_d_ / total *100.0;
            *ulti_o_d_ = *ulti_o_d_ / total *100.0;
            *ulti_n_d_ = *ulti_n_d_ / total *100.0;
            *ulti_s_d_ = *ulti_s_d_ / total *100.0;
            *ulti_cl_d_ = *ulti_cl_d_ / total *100.0;
            *ulti_ash_d_ = *ulti_ash_d_ / total *100.0;
            
            
            *prox_ash_d_ = *ulti_ash_d_ * (1.0 - *prox_h2o_d_ / 100.0);
            total = *prox_h2o_d_ + *prox_vm_d_ + *prox_ash_d_;
            *prox_fc_d_ = 100.0 - total;
        }
    }
    return true;
    
}

//////////////////////////////////////////////////////////////////
void FuelDialog::OnChooseCoal(wxCommandEvent& event)
{
    int sel;
    
    sel=combo_coal->GetSelection();
    
    *coal_name_s_=coal_name_v[sel];
    *ulti_c_d_=ulti_c_v[sel];
    *ulti_h_d_=ulti_h_v[sel];
    *ulti_o_d_=ulti_o_v[sel];
    *ulti_n_d_=ulti_n_v[sel];
    *ulti_s_d_=ulti_s_v[sel];
    *ulti_cl_d_=ulti_cl_v[sel];
    *ulti_ash_d_=ulti_ash_v[sel];
    *prox_h2o_d_=prox_h2o_v[sel];
    *prox_vm_d_=prox_vm_v[sel];
    *prox_ash_d_=prox_ash_v[sel];
    *prox_fc_d_=prox_fc_v[sel];
    *ashc_sio2_d_=ashc_sio2_v[sel];
    *ashc_al2o3_d_=ashc_al2o3_v[sel];
    *ashc_tio2_d_=ashc_tio2_v[sel];
    *ashc_fe2o3_d_=ashc_fe2o3_v[sel];
    *ashc_cao_d_=ashc_cao_v[sel];
    *ashc_mgo_d_=ashc_mgo_v[sel];
    *ashc_na2o_d_=ashc_na2o_v[sel];
    *ashc_k2o_d_=ashc_k2o_v[sel];
    *ashc_so3_d_=ashc_so3_v[sel];
    *ashc_p2o5_d_=ashc_p2o5_v[sel];
    *ashc_bao_d_=ashc_bao_v[sel];
    *ashc_sro_d_=ashc_sro_v[sel];
    *hhv_d_=hhv_v[sel];
    *coal_cost_d_ = coal_cost_v[sel];    
    
    TransferDataToWindow();
    
}

//////////////////////////////////////////////////////////
void FuelDialog::OnSaveCoal(wxCommandEvent& event)
{
    
    
}

//////////////////////////////////////////////////////////
void FuelDialog::OnNormUlti(wxCommandEvent& event)
{
    double total;
    
    
    TransferDataFromWindow();
    
    
    total =  *ulti_c_d_ + *ulti_h_d_ + *ulti_o_d_
        + *ulti_n_d_ + *ulti_s_d_ + *ulti_cl_d_ + *ulti_ash_d_;
    
    
    if (fabs(total-100.0)>1E-10)
    {
        *ulti_c_d_ = *ulti_c_d_ / total *100.0;
        *ulti_h_d_ = *ulti_h_d_ / total *100.0;
        *ulti_o_d_ = *ulti_o_d_ / total *100.0;
        *ulti_n_d_ = *ulti_n_d_ / total *100.0;
        *ulti_s_d_ = *ulti_s_d_ / total *100.0;
        *ulti_cl_d_ = *ulti_cl_d_ / total *100.0;
        *ulti_ash_d_ = *ulti_ash_d_ / total *100.0;
        
        *prox_ash_d_ = *ulti_ash_d_ * (1.0 - *prox_h2o_d_ / 100.0);
        total = *prox_h2o_d_ + *prox_vm_d_ + *prox_ash_d_;
        *prox_fc_d_ = 100.0 - total;
    }
    TransferDataToWindow();
}

//////////////////////////////////////////////////////////////
void FuelDialog::OnCalcProx(wxCommandEvent& event)
{ 
    TransferDataFromWindow();
    *prox_ash_d_ = *ulti_ash_d_ * (1.0 - *prox_h2o_d_ / 100.0);
    double total = *prox_h2o_d_ + *prox_vm_d_ + *prox_ash_d_;
    *prox_fc_d_ = 100.0 - total;
    TransferDataToWindow();
}

/////////////////////////////////////////////////////////////
void FuelDialog::OnNormAsh(wxCommandEvent& event)
{
    double total;
    
    
    TransferDataFromWindow();
    
    total = *ashc_sio2_d_+*ashc_al2o3_d_+*ashc_tio2_d_+*ashc_fe2o3_d_
        +*ashc_cao_d_+*ashc_mgo_d_+*ashc_na2o_d_+*ashc_k2o_d_+*ashc_so3_d_
        +*ashc_p2o5_d_+*ashc_bao_d_+*ashc_sro_d_;
    
    
    if (fabs(total-100.0)>1E-10)
    {
        *ashc_sio2_d_ = *ashc_sio2_d_ / total *100.0;
        *ashc_al2o3_d_ = *ashc_al2o3_d_ / total *100.0;
        *ashc_tio2_d_ = *ashc_tio2_d_ / total *100.0;
        *ashc_fe2o3_d_ = *ashc_fe2o3_d_ / total *100.0;
        *ashc_cao_d_ = *ashc_cao_d_ / total *100.0;
        *ashc_mgo_d_ = *ashc_mgo_d_ / total *100.0;
        *ashc_na2o_d_ = *ashc_na2o_d_ / total *100.0;
        *ashc_k2o_d_ = *ashc_k2o_d_ / total *100.0;
        *ashc_so3_d_ = *ashc_so3_d_ / total *100.0;
        *ashc_p2o5_d_ = *ashc_p2o5_d_ / total *100.0;
        *ashc_bao_d_ = *ashc_bao_d_ / total *100.0;
        *ashc_sro_d_ = *ashc_sro_d_ / total *100.0;
    }
    TransferDataToWindow();
}

//////////////////////////////////////////////////////////////
void FuelDialog::double2entry(wxTextCtrl* entry, double * value)
{
    wxString txt;
    char str[256];
    sprintf(str, "%.2lf",*value); 
    //txt<<(*value);
    txt=str;
    entry->SetValue(txt);
}

//////////////////////////////////////////////////////////////
void FuelDialog::entry2double(wxTextCtrl* entry, double * value)
{
    wxString txt;
    txt=entry->GetValue();
    (*value) = atof(txt.c_str());
}

//////////////////////////////////////////////////////////////
FuelDialog::~FuelDialog()
{
}

//////////////////////////////////////////////////////////////
void FuelDialog::ChangeCost2(wxCommandEvent& event)
{
    double cost_del,cost_del_mbtu;
	
    wxString txt,txt1;
    
    if (notready)
        return;
    if (hhv!=NULL&&coal_cost!=NULL&&coal_cost2!=NULL)
    {
        entry2double(hhv, hhv_d_);
        entry2double(coal_cost, coal_cost_d_);
        
        cost_del = (*coal_cost_d_)+8.66;
        cost_del_mbtu = (cost_del) / 2000.0 *1000000.0 /(*hhv_d_);
        txt<<cost_del;
		
        char buf[100];
        sprintf(buf,"%4.2f",(float)(cost_del_mbtu));
        txt1<<buf;
        coal_cost3->SetValue(txt);
        coal_cost2->SetValue(txt1);
    }
    else
        return;
}

////////////////////////////////////////////////////////////////
void FuelDialog::readDatabase(std::string file)
{
	
    FILE *db;
    int i, coal_count;
    char line[1024];
    char name[256];
    
    
    coal_name_v.clear();
    ulti_c_v.clear();
    ulti_h_v.clear();
    ulti_o_v.clear();
    ulti_n_v.clear();
    ulti_s_v.clear();
    ulti_cl_v.clear();
    ulti_ash_v.clear();
    prox_h2o_v.clear();
    prox_vm_v.clear();
    prox_ash_v.clear();
    prox_fc_v.clear();
    ashc_sio2_v.clear();
    ashc_al2o3_v.clear();
    ashc_tio2_v.clear();
    ashc_fe2o3_v.clear();
    ashc_cao_v.clear();
    ashc_mgo_v.clear();
    ashc_na2o_v.clear();
    ashc_k2o_v.clear();
    ashc_so3_v.clear();
    ashc_p2o5_v.clear();
    ashc_bao_v.clear();
    ashc_sro_v.clear();
    hhv_v.clear();
    coal_cost_v.clear();
    
    
    db = fopen(file.c_str(), "r");
	
    if(db==NULL){
        wxMessageBox("Unable to read coal database", "Error");
        return;
    }
    
    fgets(line, 1024, db);
    sscanf(line, "%d", &coal_count); 
    
    
    double ulti_c, ulti_h, ulti_o, ulti_n, ulti_s, ulti_cl, ulti_ash;
    double prox_ash, prox_h2o, prox_vm, prox_fc;
    double hhv, coal_cost;
    double ashc_sio2, ashc_al2o3, ashc_tio2, ashc_fe2o3, ashc_cao,
        ashc_mgo, ashc_na2o, ashc_k2o, ashc_so3, ashc_p2o5,
        ashc_bao, ashc_sro;
    
    
    for (i=0; i < coal_count; i++)
    {
        fgets(line, 1024, db);
        sscanf(line, "%s", name);
        
        coal_name_v.push_back(name);
        
        fgets(line, 1024, db);
        sscanf(line, "%lf %lf %lf %lf %lf %lf %lf", 
            &ulti_c,
            &ulti_h,
            &ulti_o,
            &ulti_n,
            &ulti_s,
            &ulti_cl,
            &ulti_ash);
        
        
        ulti_c_v.push_back(ulti_c);
        ulti_h_v.push_back(ulti_h);
        ulti_o_v.push_back(ulti_o);
        ulti_n_v.push_back(ulti_n);
        ulti_s_v.push_back(ulti_s);
        ulti_cl_v.push_back(ulti_cl);
        ulti_ash_v.push_back(ulti_ash);
        
        
        fgets(line, 1024, db);
        sscanf(line, "%lf %lf %lf %lf",
            &prox_ash,
            &prox_h2o,
            &prox_vm,
            &prox_fc);
        
        prox_ash_v.push_back(prox_ash);
        prox_h2o_v.push_back(prox_h2o);
        prox_vm_v.push_back(prox_vm);
        prox_fc_v.push_back(prox_fc);
        
        
        fgets(line, 1024, db);
        sscanf(line, "%lf", &hhv);
        hhv_v.push_back(hhv);
        
        fgets(line, 1024, db);
        sscanf(line, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
            &ashc_sio2,
            &ashc_al2o3,
            &ashc_tio2,
            &ashc_fe2o3,
            &ashc_cao,
            &ashc_mgo,
            &ashc_na2o,
            &ashc_k2o,
            &ashc_so3,
            &ashc_p2o5,
            &ashc_bao,
            &ashc_sro);
        
        
        ashc_sio2_v.push_back(ashc_sio2);
        ashc_al2o3_v.push_back(ashc_al2o3);
        ashc_tio2_v.push_back(ashc_tio2);
        ashc_fe2o3_v.push_back(ashc_fe2o3);
        ashc_cao_v.push_back(ashc_cao);
        ashc_mgo_v.push_back(ashc_mgo);
        ashc_na2o_v.push_back(ashc_na2o);
        ashc_k2o_v.push_back(ashc_k2o);
        ashc_so3_v.push_back(ashc_so3);
        ashc_p2o5_v.push_back(ashc_p2o5);
        ashc_bao_v.push_back(ashc_bao);
        ashc_sro_v.push_back(ashc_sro);
        
        fgets(line, 1024, db);
        sscanf(line, "%lf", &coal_cost);
        coal_cost_v.push_back(coal_cost);
        
        
    }
    
    fclose(db);
    
    
    if(*coal_name_s_=="Noname Coal") {
		
		int sel=0;

        *coal_name_s_=coal_name_v[sel];
		*ulti_c_d_=ulti_c_v[sel];
		*ulti_h_d_=ulti_h_v[sel];
		*ulti_o_d_=ulti_o_v[sel];
		*ulti_n_d_=ulti_n_v[sel];
		*ulti_s_d_=ulti_s_v[sel];
		*ulti_cl_d_=ulti_cl_v[sel];
		*ulti_ash_d_=ulti_ash_v[sel];
		*prox_h2o_d_=prox_h2o_v[sel];
		*prox_vm_d_=prox_vm_v[sel];
		*prox_ash_d_=prox_ash_v[sel];
		*prox_fc_d_=prox_fc_v[sel];
		*ashc_sio2_d_=ashc_sio2_v[sel];
		*ashc_al2o3_d_=ashc_al2o3_v[sel];
		*ashc_tio2_d_=ashc_tio2_v[sel];
		*ashc_fe2o3_d_=ashc_fe2o3_v[sel];
		*ashc_cao_d_=ashc_cao_v[sel];
		*ashc_mgo_d_=ashc_mgo_v[sel];
		*ashc_na2o_d_=ashc_na2o_v[sel];
		*ashc_k2o_d_=ashc_k2o_v[sel];
		*ashc_so3_d_=ashc_so3_v[sel];
		*ashc_p2o5_d_=ashc_p2o5_v[sel];
		*ashc_bao_d_=ashc_bao_v[sel];
		*ashc_sro_d_=ashc_sro_v[sel];
		*hhv_d_=hhv_v[sel];
		*coal_cost_d_ = coal_cost_v[sel];       
    }
    
    
} 