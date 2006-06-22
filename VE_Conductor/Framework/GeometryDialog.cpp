/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: GeometryDialog.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "GeometryDialog.h"

#include <wx/window.h>
#include <wx/radiobox.h>
#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/listbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

BEGIN_EVENT_TABLE(GeometryDialog, wxDialog)
   EVT_BUTTON(GEOMETRY_ADDNEWPAGE,GeometryDialog::_onButtonAddNewGeomInfoPackage)
   EVT_BUTTON(GEOMETRY_SAVENEWPAGE,GeometryDialog::_onButtonSaveGeomInfoPackage)
   EVT_UPDATE_UI(GEOMETRY_SAVENEWPAGE,GeometryDialog::_onUIUpdateButtonSaveGeomInfoPackage)
   EVT_BUTTON(GEOMETRY_DELETEPAGES,GeometryDialog::_onButtonDeleteSelGeomInfoPackage)
   EVT_UPDATE_UI(GEOMETRY_DELETEPAGES,GeometryDialog::_onUIUpdateButtonDeleteSelGeomInfoPackage)
   EVT_LISTBOX(GEOMETRY_LISTBOX, GeometryDialog::_onListBox)
   EVT_LISTBOX_DCLICK(GEOMETRY_LISTBOX, GeometryDialog::_onDClickListBox)
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

   _buildPage();

  
   //_buildPage();
}

void GeometryDialog::_buildPage()
{
   
   //Sketch Design
   wxSizer *whole_sizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* left_margin = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* middle_sizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* right_margin = new wxBoxSizer(wxHORIZONTAL);
   
   wxBoxSizer* topmiddle_sizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* bottommiddle_sizer = new wxBoxSizer(wxHORIZONTAL);

   left_margin->Add(5, 10);
   right_margin->Add(5, 10);
   whole_sizer->Add(left_margin, 0, wxALIGN_LEFT);
   whole_sizer->Add(middle_sizer, 0,  wxALIGN_CENTER_HORIZONTAL);
   whole_sizer->Add(right_margin, 0, wxALIGN_RIGHT);

   wxStaticBox *box =  new wxStaticBox(this, -1, _T("Add Panel"));
   wxSizer *topmiddleleft_sizer = new wxStaticBoxSizer(box,wxVERTICAL);
   
   box = new wxStaticBox(this, -1, _T("Geom Info"));
   wxSizer *topmiddleright_sizer = new wxStaticBoxSizer(box,wxVERTICAL);
  
   topmiddle_sizer->Add(topmiddleleft_sizer,0);
   topmiddle_sizer->Add(topmiddleright_sizer,0);

   middle_sizer->Add(10,10,0); //the top margin
   middle_sizer->Add(topmiddle_sizer,0);
   middle_sizer->Add(10,5,0);
   middle_sizer->Add(bottommiddle_sizer,0, wxALIGN_CENTER_HORIZONTAL);
   middle_sizer->Add(10,10,0); //the bottom margin
    
   //Top left design
   lbox_geompackagenames = new wxListBox(this, GEOMETRY_LISTBOX,
                           wxDefaultPosition, wxDefaultSize,
                           0,NULL,
                           wxLB_HSCROLL);

   wxBoxSizer* add_delete_row = new wxBoxSizer(wxHORIZONTAL);
   
   topmiddleleft_sizer->Add(lbox_geompackagenames, 0, wxGROW | wxALL, 5);
   topmiddleleft_sizer->Add(10,65,0);
   topmiddleleft_sizer->Add(add_delete_row,0,wxGROW | wxALL, 5);
   topmiddleleft_sizer->SetMinSize(150,0); 
      
      
   add_button = new wxButton(this, GEOMETRY_ADDNEWPAGE, _T("Add"));
   save_button = new wxButton(this, GEOMETRY_SAVENEWPAGE, _T("Save"));
   delete_button = new wxButton (this, GEOMETRY_DELETEPAGES, _T("Delete"));

   add_delete_row->Add(add_button, 0, wxALIGN_CENTER_HORIZONTAL);
   add_delete_row->Add(10,3,0);
   add_delete_row->Add(save_button, 0, wxALIGN_CENTER_HORIZONTAL);
   add_delete_row->Add(10,3,0);
   add_delete_row->Add(delete_button, 0, wxALIGN_CENTER_HORIZONTAL);

   //Top right design
   wxBoxSizer *data_zero_row = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *data_first_row = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *data_second_row = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *data_third_row = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *data_fourth_row = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *data_fifth_row = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *data_sixth_row = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *data_seventh_row = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *data_eighth_row = new wxBoxSizer(wxHORIZONTAL);

   topmiddleright_sizer->Add(10, 5, 0);
   topmiddleright_sizer->Add(data_zero_row, 0);
   topmiddleright_sizer->Add(10, 3, 0);
   topmiddleright_sizer->Add(data_first_row, 0);
   topmiddleright_sizer->Add(10, 3, 0);
   topmiddleright_sizer->Add(data_second_row, 0);
   topmiddleright_sizer->Add(10, 3, 0);
   topmiddleright_sizer->Add(data_third_row, 0);
   topmiddleright_sizer->Add(10, 3, 0);
   topmiddleright_sizer->Add(data_fourth_row,0);
   topmiddleright_sizer->Add(10, 3, 0);
   topmiddleright_sizer->Add(data_fifth_row,0);
   topmiddleright_sizer->Add(10, 3, 0);
   topmiddleright_sizer->Add(data_sixth_row,0);
   topmiddleright_sizer->Add(10, 3, 0);
   topmiddleright_sizer->Add(data_seventh_row,0);
   topmiddleright_sizer->Add(10, 3, 0);
   topmiddleright_sizer->Add(data_eighth_row,0);
  

   bottommiddle_sizer->Add(new wxButton(this, wxID_OK, "OK"), 0, wxALIGN_CENTER_HORIZONTAL);
   bottommiddle_sizer->Add(200,15,0);
   bottommiddle_sizer->Add(new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALIGN_CENTER_HORIZONTAL);
  
   wxStaticText * label0 = new wxStaticText(this, -1, " Geom Name : ", wxDefaultPosition, wxSize(200, 17));
   t_geomname = new wxTextCtrl(this, -1, wxT("0"), wxDefaultPosition, wxSize(80, 20));
   data_zero_row->Add(label0);
   data_zero_row->Add(t_geomname);
  
   wxStaticText* label1 = new wxStaticText(this, -1, "Geom File Name: ", wxDefaultPosition, wxSize(200, 17));
   t_geomfilename = new wxTextCtrl(this, -1, wxT("9"), wxDefaultPosition, wxSize(80, 20));
   data_first_row->Add(label1);
   data_first_row->Add(t_geomfilename);
  

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

   SetSizer(whole_sizer);
   SetAutoLayout(TRUE);
   whole_sizer->Fit(this);  


}

void GeometryDialog::_onButtonAddNewGeomInfoPackage(wxCommandEvent& WXUNUSED(event))
{
   std::vector<GeometryInfoPackage> templist;
   templist = geometryDataBuffer->GetCurrentGeomInfoList();
  
   GeometryInfoPackage newGeometryInfoPackage;
   newGeometryInfoPackage =  geometryDataBuffer->GetDefaultNewGeomInfoPackage(templist.size());

   templist.push_back(newGeometryInfoPackage); 

   //wxString s = newGeometryInfoPackage.GetGeomName().c_str();
   //lbox_geompackagenames->Append(s);
   _onUpdateUIInfoPage(templist,templist.size()-1);
   wxArrayString items;
   std::string tempname;
   
   for( unsigned int i=0; i<templist.size();i++)
   {
         tempname = templist[i].GetGeomName();
         items.Add(tempname.c_str());
   }
   lbox_geompackagenames->Set(items);
 
}

void GeometryDialog::_onButtonSaveGeomInfoPackage(wxCommandEvent& WXUNUSED(event))
{
   GeometryInfoPackage temppackage;
   temppackage =  GetGeomInfoPackageFromInfoPage();
   
   wxArrayInt selections;
   int n = lbox_geompackagenames->GetSelections(selections);
   int item = selections[0];//right now can only selecte one item at a time

   std::vector<GeometryInfoPackage> templist;
   templist = geometryDataBuffer->GetCurrentGeomInfoList();

   if( static_cast< int >( templist.size() ) < lbox_geompackagenames->GetCount())//this is a new package, add to the list
   {
      geometryDataBuffer->AddGeomInfoToCurrentList(temppackage);

   }
   else //this is an old, just update the list
   {
      geometryDataBuffer->UpdateGeomInfoToCurrentList(temppackage,item);
   }
 
   templist = geometryDataBuffer->GetCurrentGeomInfoList();

   _onUpdateUIInfoPage(templist,item);
   
   wxArrayString items;
   std::string tempname;
   
   for( unsigned int i=0; i<templist.size();i++)
   {
      tempname = templist[i].GetGeomName();
      items.Add(tempname.c_str());
   }
   lbox_geompackagenames->Set(items);

}

void GeometryDialog::_onUIUpdateButtonSaveGeomInfoPackage(wxUpdateUIEvent& event )
{
   wxArrayInt selections;
   event.Enable(lbox_geompackagenames->GetSelections(selections) != 0);

}

void GeometryDialog::_onButtonDeleteSelGeomInfoPackage(wxCommandEvent& WXUNUSED(event))
{
   wxArrayInt selections;
   std::vector<int> itemindexs;
   
   int n = lbox_geompackagenames->GetSelections(selections);
   while (n>0)
   {
      lbox_geompackagenames->Delete(selections[--n]);
      itemindexs.push_back(selections[n]);
   }
   geometryDataBuffer->DeleteGeomInfosFromCurrentList(itemindexs);
    
   std::vector<GeometryInfoPackage> templist;
   templist = geometryDataBuffer->GetCurrentGeomInfoList();
   _onUpdateUIInfoPage(templist,0);
   
   wxArrayString items;
   std::string tempname;
   
   for ( unsigned int i=0; i<templist.size();i++)
   {
      tempname = templist[i].GetGeomName();
      items.Add(tempname.c_str());
   }
   lbox_geompackagenames->Set(items);

}


void GeometryDialog::_onUIUpdateButtonDeleteSelGeomInfoPackage(wxUpdateUIEvent& event )
{
    wxArrayInt selections;
    event.Enable(lbox_geompackagenames->GetSelections(selections) != 0);
}

void GeometryDialog::_onListBox(wxCommandEvent& event)
{
   int item = event.GetInt();
   std::vector<GeometryInfoPackage> templist;
   templist = geometryDataBuffer->GetCurrentGeomInfoList();
   _onUpdateUIInfoPage(templist,item);
}

void GeometryDialog::_onDClickListBox(wxCommandEvent& WXUNUSED(event))
{
}

bool GeometryDialog::TransferDataFromWindow()
{
   return true;
}

void GeometryDialog::SetGeometryDataBuffer( GeometryDataBuffer* input )
{
   geometryDataBuffer = input;
}

bool GeometryDialog::TransferDataToWindow()
{
   //if this is an old dialog, get the data from databuffer

   std::vector<GeometryInfoPackage> templist = geometryDataBuffer->GetCurrentGeomInfoList();
  if(templist.size()>0)
  {
      _onUpdateUIInfoPage(templist, 0); //by default, the geominfo page shows the first package.
      wxArrayString items;
      std::string tempname;
      for ( unsigned int i=0; i<templist.size();i++)
      {
         tempname = templist[i].GetGeomName();
         items.Add(tempname.c_str());
      }
   
      lbox_geompackagenames->Set(items);

  }
      
   return true;

}

void GeometryDialog::_onUpdateUIInfoPage(std::vector<GeometryInfoPackage> templist, int index)
{

   //TODO
   //By default when the GeometryDialog is actived, the GeomInfo is pointed to the first geompackage the module has.
   int tempSize = static_cast< int >( templist.size() );
   if( (tempSize > 0) && (index < tempSize) )
   {
      wxString temp_string;
      temp_string<<templist[index].GetGeomName().c_str();
      t_geomname->SetValue(temp_string);

      temp_string.Clear();
      temp_string<<templist[index].GetGeomFileName().c_str();
      t_geomfilename->SetValue(temp_string);
   
      temp_string.Clear();
      temp_string<<templist[index].GetTransparencyToggle();
      t_transparencytoggle->SetValue(temp_string);

      temp_string.Clear();
      temp_string<<templist[index].GetColorFlag();
      t_colorflag->SetValue(temp_string);

      temp_string.Clear();
      temp_string<<templist[index].GetScales()[0];
      t_scale0->SetValue(temp_string);
   
      temp_string.Clear();
      temp_string<<templist[index].GetScales()[1];
      t_scale1->SetValue(temp_string);
 
      temp_string.Clear();
      temp_string<<templist[index].GetScales()[2];
      t_scale2->SetValue(temp_string);

      temp_string.Clear();
      temp_string<<templist[index].GetTrans()[0];
      t_tran0->SetValue(temp_string);
   
      temp_string.Clear();
      temp_string<<templist[index].GetTrans()[1];
      t_tran1->SetValue(temp_string);
 
      temp_string.Clear();
      temp_string<<templist[index].GetTrans()[2];
      t_tran2->SetValue(temp_string);

      temp_string.Clear();
      temp_string<<templist[index].GetRots()[0];
      t_rot0->SetValue(temp_string);
   
      temp_string.Clear();
      temp_string<<templist[index].GetRots()[1];
      t_rot1->SetValue(temp_string);
 
      temp_string.Clear();
      temp_string<<templist[index].GetRots()[2];
      t_rot2->SetValue(temp_string);

      temp_string.Clear();
      temp_string<<templist[index].GetColors()[0];
      t_color0->SetValue(temp_string);
   
      temp_string.Clear();
      temp_string<<templist[index].GetColors()[1];
      t_color1->SetValue(temp_string);
 
      temp_string.Clear();
      temp_string<<templist[index].GetColors()[2];
      t_color2->SetValue(temp_string);

      temp_string.Clear();
      temp_string<<templist[index].GetLOD();
      t_LOD->SetValue(temp_string);

     
   }
   else //there is no geominfo in the list
   {
      t_geomname->SetValue("");
      t_geomfilename->SetValue("");
      t_transparencytoggle->SetValue("");
      t_colorflag->SetValue("");
      t_scale0->SetValue("");
      t_scale1->SetValue("");
      t_scale2->SetValue("");
      t_tran0->SetValue("");
      t_tran1->SetValue("");
      t_tran2->SetValue("");
      t_rot0->SetValue("");
      t_rot1->SetValue("");
      t_rot2->SetValue("");
      t_color0->SetValue("");
      t_color1->SetValue("");
      t_color2->SetValue("");
      t_LOD->SetValue("");
   }

}

GeometryInfoPackage GeometryDialog::GetGeomInfoPackageFromInfoPage()
{
   GeometryInfoPackage temppackage;
   
   
   wxString txt;
   txt = t_geomname->GetValue();
   temppackage.SetGeomName(txt.c_str());
   
   txt = t_geomfilename->GetValue();
   temppackage.SetGeomFileName(txt.c_str());

   txt = t_transparencytoggle->GetValue();
   temppackage.SetTransparencyToggle((atoi(txt.c_str())==0)?false:true);
//   temppackage.SetTransparencyToggle((bool)atoi(txt.c_str()));

   txt = t_colorflag->GetValue();
   temppackage.SetColorFlag((atoi(txt.c_str())==0)?false:true);
//   temppackage.SetColorFlag((bool)atoi(txt.c_str()));
   double x, y,z;
   txt = t_scale0->GetValue();
   x = (double) atof(txt.c_str());
   txt = t_scale1->GetValue();
   y = (double) atof(txt.c_str());
   txt = t_scale2->GetValue();
   z= (double) atof(txt.c_str());
   temppackage.SetScales(x,y,z);


   txt = t_tran0->GetValue();
   x = (double) atof(txt.c_str());
   txt = t_tran1->GetValue();
   y = (double) atof(txt.c_str());
   txt = t_tran2->GetValue();
   z= (double) atof(txt.c_str());
   temppackage.SetTrans(x,y,z);


   txt = t_rot0->GetValue();
   x = (double) atof(txt.c_str());
   txt = t_rot1->GetValue();
   y = (double) atof(txt.c_str());
   txt = t_rot2->GetValue();
   z= (double) atof(txt.c_str());
   temppackage.SetRots(x,y,z);


   txt = t_color0->GetValue();
   x = (double) atof(txt.c_str());
   txt = t_color1->GetValue();
   y = (double) atof(txt.c_str());
   txt = t_color2->GetValue();
   z= (double) atof(txt.c_str());
   temppackage.SetColors(x,y,z);
  
   txt = t_LOD->GetValue();
   temppackage.SetLOD((double) atof(txt.c_str()));

   return temppackage;

}


