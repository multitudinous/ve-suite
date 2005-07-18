#include "GeometryDialog.h"
   
   
BEGIN_EVENT_TABLE(GeometryDialog, wxDialog)
   EVT_COMMAND_SCROLL(GEOMETRY_CONFIG_OPACITY_SLIDER, GeometryDialog::ChangeOpacity)
   EVT_COMMAND_SCROLL(GEOMETRY_CONFIG_LOD_SLIDER, GeometryDialog::_onGeometry)
   EVT_CHECKLISTBOX(GEOMETRY_CONFIG_CBOX,GeometryDialog::_onUpdate)
   //EVT_RADIOBOX(GEOMETRY_CONFIG_RBOX,Geometry::ChangeOpacity)
   EVT_BUTTON(GEOMETRY_CONFIG_UPDATE_BUTTON,GeometryDialog::_onUpdate)
END_EVENT_TABLE()


GeometryDialog::GeometryDialog( wxWindow *parent,
                                wxWindowID id,
                                const wxString &title,
                                const wxPoint  &position,
                                const wxSize &size,
                                long style
                                ) : wxDialog( parent, id, title, position, size,style)

{
   
   _geometryRBox = 0;
   _geometryCBox = 0;
   _updateButton = 0;
   geomOpacitySlider = 0;
   geomLODSlider = 0;
   _parent = parent;
   

   
  //GeometryDataManager::getInstance().GetGeometryDataBuffer()->InitializeNewGeomInfo(0);

  wxBoxSizer* toptop= new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);
  
  left_margin->Add(5, 10);
  right_margin->Add(5, 10);
  toptop->Add(left_margin, 0, wxALIGN_LEFT);
  toptop->Add(top_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
  toptop->Add(right_margin, 0, wxALIGN_RIGHT);

  wxBoxSizer* data_row = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* ok_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(10, 10, 0); //the top margin
  top_sizer->Add(data_row, 0); 
  top_sizer->Add(10, 5, 0);
  top_sizer->Add(ok_row, 0, wxALIGN_CENTER_HORIZONTAL);
  top_sizer->Add(10, 10, 0); //the bottom margin
  
  wxBoxSizer *data_zero_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_fourth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_fifth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_sixth_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_seventh_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *data_eighth_row = new wxBoxSizer(wxHORIZONTAL);

  data_row->Add(10, 5, 0);
  data_row->Add(data_zero_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_first_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_second_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_third_row, 0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_fourth_row,0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_fifth_row,0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_sixth_row,0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_seventh_row,0);
  data_row->Add(10, 3, 0);
  data_row->Add(data_eighth_row,0);
  

  ok_row->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
  ok_row->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
  wxStaticText* label0 = new wxStaticText(this, -1, "Geom File Name: ", wxDefaultPosition, wxSize(200, 17));
  t_geomfilename = new wxTextCtrl(this, -1, wxT("9"), wxDefaultPosition, wxSize(80, 20));
  data_zero_row->Add(label0);
  data_zero_row->Add(t_geomfilename);
  
  wxStaticText * label1 = new wxStaticText(this, -1, " Geom Name : ", wxDefaultPosition, wxSize(200, 17));
  t_geomname = new wxTextCtrl(this, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  data_first_row->Add(label1);
  data_first_row->Add(t_geomname);

  wxStaticText * label2 = new wxStaticText(this, -1, " Transparency Toggle : ", wxDefaultPosition, wxSize(200, 17));
  t_transparencytoggle = new wxTextCtrl(this, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
  data_second_row->Add(label2);
  data_second_row->Add(t_transparencytoggle);

  wxStaticText * label3 = new wxStaticText(this, -1, " Color Flag : ", wxDefaultPosition, wxSize(200, 17));
  t_colorflag = new wxTextCtrl(this, -1, wxT("1"), wxDefaultPosition, wxSize(80, 20));
  data_third_row->Add(label3);
  data_third_row->Add(t_colorflag);

  wxStaticText *label4 = new wxStaticText(this, -1, "Scale Array: ", wxDefaultPosition, wxSize(200, 17));
  t_scale0 = new wxTextCtrl(this, -1, wxT("1.0"), wxDefaultPosition, wxSize(80,20));
  t_scale1 = new wxTextCtrl(this, -1, wxT("1.0"), wxDefaultPosition, wxSize(80,20));
  t_scale2 = new wxTextCtrl(this, -1, wxT("1.0"), wxDefaultPosition, wxSize(80,20));
  data_fourth_row->Add(label4);
  data_fourth_row->Add(t_scale0);
  data_fourth_row->Add(t_scale1);
  data_fourth_row->Add(t_scale2);
  
wxStaticText *label5 = new wxStaticText(this, -1, "Translation Array: ", wxDefaultPosition, wxSize(200, 17));
  t_tran0 = new wxTextCtrl(this, -1, wxT("0.0"), wxDefaultPosition, wxSize(80,20));
  t_tran1 = new wxTextCtrl(this, -1, wxT("0.0"), wxDefaultPosition, wxSize(80,20));
  t_tran2 = new wxTextCtrl(this, -1, wxT("0.0"), wxDefaultPosition, wxSize(80,20));
  data_fifth_row->Add(label5);
  data_fifth_row->Add(t_tran0);
  data_fifth_row->Add(t_tran1);
  data_fifth_row->Add(t_tran2);


wxStaticText *label6 = new wxStaticText(this, -1, "Rotation Array: ", wxDefaultPosition, wxSize(200, 17));
  t_rot0 = new wxTextCtrl(this, -1, wxT("0.0"), wxDefaultPosition, wxSize(80,20));
  t_rot1 = new wxTextCtrl(this, -1, wxT("0.0"), wxDefaultPosition, wxSize(80,20));
  t_rot2 = new wxTextCtrl(this, -1, wxT("0.0"), wxDefaultPosition, wxSize(80,20));
  data_sixth_row->Add(label6);
  data_sixth_row->Add(t_rot0);
  data_sixth_row->Add(t_rot1);
  data_sixth_row->Add(t_rot2);

 wxStaticText *label7 = new wxStaticText(this, -1, "RGB Array: ", wxDefaultPosition, wxSize(200, 17));
  t_color0 = new wxTextCtrl(this, -1, wxT("1.0"), wxDefaultPosition, wxSize(80,20));
  t_color1 = new wxTextCtrl(this, -1, wxT("0.0"), wxDefaultPosition, wxSize(80,20));
  t_color2 = new wxTextCtrl(this, -1, wxT("0.0"), wxDefaultPosition, wxSize(80,20));
  data_seventh_row->Add(label7);
  data_seventh_row->Add(t_color0);
  data_seventh_row->Add(t_color1);
  data_seventh_row->Add(t_color2);

   wxStaticText * label8 = new wxStaticText(this, -1, " LOD : ", wxDefaultPosition, wxSize(200, 17));
  t_LOD = new wxTextCtrl(this, -1, wxT("0.5"), wxDefaultPosition, wxSize(80, 20));
  data_eighth_row->Add(label8);
  data_eighth_row->Add(t_LOD);

  SetSizer(toptop);
  SetAutoLayout(TRUE);
  toptop->Fit(this);  


   //_buildPage();
}

void GeometryDialog::_onGeometry( wxScrollEvent& WXUNUSED(event) )
{

}

//////////////////////////////////////////////////
void GeometryDialog::ChangeOpacity( wxScrollEvent& WXUNUSED(event) )
{

}

//////////////////////////////////////////////////
void GeometryDialog::_onUpdate(wxCommandEvent& WXUNUSED(event) )
{

}



bool GeometryDialog::TransferDataFromWindow()
{

   std::vector<GeometryInfoPackage> templist = GeometryDataManager::getInstance().GetGeometryDataBuffer()->GetCurrentModuleGeomInfoListFromMap();


   wxString txt;
   txt = t_geomfilename->GetValue();
   templist[0].SetGeomFileName(txt.c_str());

   txt = t_transparencytoggle->GetValue();
   templist[0].SetTransparencyToggle((bool)atoi(txt.c_str()));

   txt = t_colorflag->GetValue();
   templist[0].SetColorFlag((bool)atoi(txt.c_str()));

   double x, y,z;
   txt = t_scale0->GetValue();
   x = (double) atof(txt.c_str());
   txt = t_scale1->GetValue();
   y = (double) atof(txt.c_str());
   txt = t_scale2->GetValue();
   z= (double) atof(txt.c_str());
   templist[0].SetScales(x,y,z);


   txt = t_tran0->GetValue();
   x = (double) atof(txt.c_str());
   txt = t_tran1->GetValue();
   y = (double) atof(txt.c_str());
   txt = t_tran2->GetValue();
   z= (double) atof(txt.c_str());
   templist[0].SetTrans(x,y,z);


   txt = t_rot0->GetValue();
   x = (double) atof(txt.c_str());
   txt = t_rot1->GetValue();
   y = (double) atof(txt.c_str());
   txt = t_rot2->GetValue();
   z= (double) atof(txt.c_str());
   templist[0].SetRots(x,y,z);


   txt = t_color0->GetValue();
   x = (double) atof(txt.c_str());
   txt = t_color1->GetValue();
   y = (double) atof(txt.c_str());
   txt = t_color2->GetValue();
   z= (double) atof(txt.c_str());
   templist[0].SetColors(x,y,z);


  
   txt = t_LOD->GetValue();
   templist[0].SetLOD((double) atof(txt.c_str()));


   GeometryDataManager::getInstance().GetGeometryDataBuffer()->SetCurrentGeomInfoList(templist);
   
   GeometryDataManager::getInstance().GetGeometryDataBuffer()->UpdateCurrentGeomInfoListToMap();

   return true;
}

bool GeometryDialog::TransferDataToWindow()
{
   //if this is an old dialog, get the data from databuffer
  
   std::vector<GeometryInfoPackage> templist = GeometryDataManager::getInstance().GetGeometryDataBuffer()->GetCurrentModuleGeomInfoListFromMap();

   
   //TODO
   //Right now every module only has one geomeinfopackage, templist.size()=0
   
   wxString temp_string;
   temp_string<<templist[0].GetGeomName().c_str();
   t_geomname->SetValue(temp_string);

   temp_string.Clear();
   temp_string<<templist[0].GetGeomFileName().c_str();
   t_geomfilename->SetValue(temp_string);
   
   temp_string.Clear();
   temp_string<<templist[0].GetTransparencyToggle();
   t_transparencytoggle->SetValue(temp_string);

   temp_string.Clear();
   temp_string<<templist[0].GetColorFlag();
   t_colorflag->SetValue(temp_string);

   temp_string.Clear();
   temp_string<<templist[0].GetScales()[0];
   t_scale0->SetValue(temp_string);
   
   temp_string.Clear();
   temp_string<<templist[0].GetScales()[1];
   t_scale1->SetValue(temp_string);
 
   temp_string.Clear();
   temp_string<<templist[0].GetScales()[2];
   t_scale2->SetValue(temp_string);

   temp_string.Clear();
   temp_string<<templist[0].GetTrans()[0];
   t_tran0->SetValue(temp_string);
   
   temp_string.Clear();
   temp_string<<templist[0].GetTrans()[1];
   t_tran1->SetValue(temp_string);
 
   temp_string.Clear();
   temp_string<<templist[0].GetTrans()[2];
   t_tran2->SetValue(temp_string);

   temp_string.Clear();
   temp_string<<templist[0].GetRots()[0];
   t_rot0->SetValue(temp_string);
   
   temp_string.Clear();
   temp_string<<templist[0].GetRots()[1];
   t_rot1->SetValue(temp_string);
 
   temp_string.Clear();
   temp_string<<templist[0].GetRots()[2];
   t_rot2->SetValue(temp_string);

   temp_string.Clear();
   temp_string<<templist[0].GetColors()[0];
   t_color0->SetValue(temp_string);
   
   temp_string.Clear();
   temp_string<<templist[0].GetColors()[1];
   t_color1->SetValue(temp_string);
 
   temp_string.Clear();
   temp_string<<templist[0].GetColors()[2];
   t_color2->SetValue(temp_string);

   temp_string.Clear();
   temp_string<<templist[0].GetLOD();
   t_LOD->SetValue(temp_string);

   
   return true;

}


