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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/GUIPlugin/CORBAServiceList.h"

#include "VE_Conductor/GUIPlugin/Plugin_base.h"
#include "VE_Conductor/GUIPlugin/CORBAServiceList.h"
#include <iostream>
#include "VE_Conductor/Network/string_ops.h"
#include "VE_Conductor/GUIPlugin/SummaryResultDialog.h"
#include "VE_Conductor/GUIPlugin/UIDialog.h"
#include "VE_Conductor/GUIPlugin/TextResultDialog.h"
#include "VE_Conductor/GUIPlugin/TexTable.h"
// EPRI TAG
#include "VE_Conductor/GUIPlugin/FinancialDialog.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"

#include "VE_Conductor/Utilities/CADNodeManagerDlg.h"

#include <wx/dc.h>
#include <wx/msgdlg.h>
#include <wx/image.h>
#include <wx/wx.h>
#include <math.h>

using namespace VE_XML::VE_Model;
using namespace VE_XML;

IMPLEMENT_DYNAMIC_CLASS( REI_Plugin, wxObject )

/////////////////////////////////////////////////////////////////////////////
REI_Plugin::REI_Plugin()
{ 
   dlg         = 0; 
   result_dlg  = 0;
   port_dlg    = 0;
   geom_dlg     = 0;
   //geometryDataBuffer = 0;
   // EPRI TAG
   financial_dlg = 0;

   pos = wxPoint(0,0); //default position
  
   //default shape
   //n_pts=5;
   //poly = new wxPoint[n_pts];
  
   //poly[0]=wxPoint(0,20);
   //poly[1]=wxPoint(20,0);
   //poly[2]=wxPoint(40,20);
   //poly[3]=wxPoint(30, 40);
   //poly[4]=wxPoint(10,40);

   wxImage my_img( square_xpm );
   icon_w = (int)my_img.GetWidth()*0.30f;
   icon_h = (int)my_img.GetHeight()*0.30f;
   my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));
   
   n_pts = 4;
   poly = new wxPoint[n_pts];
   poly[0]=wxPoint(0,0);
   poly[1]=wxPoint(icon_w,0);
   poly[2]=wxPoint(icon_w,icon_h);
   poly[3]=wxPoint(0,icon_h);
  
   //mod_pack._type = 1 ; //Module
   //mod_pack._category = 1; // normal modules
   //mod_pack._id = -1;
   veModel = new Model();
   numberOfInputPorts = 0;
   numberOfOutputPorts = 0;

   inputsDialog = 0;
   resultsDialog = 0;
   portsDialog = 0;
   
   iconFilename = "DefaultPlugin";
   
   defaultIconMap[ "contour.xpm" ] = wxImage( contour_xpm );
   defaultIconMap[ "isosurface.xpm" ] = wxImage( isosurface_xpm );
   defaultIconMap[ "ROItb.xpm" ] = wxImage( ROItb_xpm );
   defaultIconMap[ "streamlines.xpm" ] = wxImage( streamlines_xpm );
   defaultIconMap[ "vector.xpm" ] = wxImage( vector_xpm );
   defaultIconMap[ "vectortb.xpm" ] = wxImage( vectortb_xpm );
}

/////////////////////////////////////////////////////////////////////////////
REI_Plugin::~REI_Plugin()
{
   delete [] poly;
   poly = 0;

   if (dlg!=NULL)
   {       
      delete dlg;
      dlg = 0;
   }
   
   if (result_dlg!=NULL)
   {
      delete result_dlg;
      result_dlg = 0;
   }
   
   if (port_dlg!=NULL)   
   {
      delete port_dlg;
      port_dlg = 0;
   }

   /*if ( geom_dlg!=NULL )
   {
      delete geom_dlg;
      geom_dlg = 0;
   }*/

   if ( veModel !=NULL )
   {
      delete veModel;
      veModel = 0;
   }

   // EPRI TAG
   if (financial_dlg!=NULL) 
   {
      delete financial_dlg;
      financial_dlg = 0;
   }

   if ( inputsDialog )
   {
      inputsDialog->Destroy();
      inputsDialog = 0;
   }

   if ( resultsDialog )
   {
      resultsDialog->Destroy();
      resultsDialog = 0;
   }

   if ( portsDialog )
   {
      portsDialog->Destroy();
      portsDialog = 0;
   }
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetID(int id)
{
   this->id = id;
}
/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetName( wxString pluginName )
{
   name = pluginName;
}
/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetPos(wxPoint pt)
{
  pos = pt;
}

/////////////////////////////////////////////////////////////////////////////
double REI_Plugin::GetVersion()
{
  return 0.1;
}

/////////////////////////////////////////////////////////////////////////////
wxRect REI_Plugin::GetBBox()
{
  wxRect result;
  //int left, right, top, bottom;
  //int i;

  //Calculate the Bounding box out the of polygon
  result.SetX(pos.x);
  result.SetY(pos.y);

   if ( n_pts == 0 )
   {
      result.SetWidth(edge_size);
      result.SetHeight(edge_size);
      return result;
   }

   int left = poly[0].x;
   int right = poly[0].x;
   int top = poly[0].y;
   int bottom = poly[0].y;

   for (int i=1; i<n_pts; i++)
   {
      if (left>poly[i].x)
         left=poly[i].x;
      
      if (right<poly[i].x)
         right=poly[i].x;
   
      if (top>poly[i].y)
         top=poly[i].y;
   
      if (bottom<poly[i].y)
         bottom=poly[i].y;
   }

   result.SetWidth(right-left+edge_size);
   result.SetHeight(bottom-top+edge_size);
   return result;	
}

/////////////////////////////////////////////////////////////////////////////
int REI_Plugin::GetNumPoly( void )
{
	return n_pts;
}
/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::GetPoly( POLY& polygon )
{
   for ( int i=0; i < n_pts; i++ )  
   {
      polygon[i]=poly[i];
   }
}

/////////////////////////////////////////////////////////////////////////////
int REI_Plugin::GetNumIports()
{
	return inputPort.size();
}
/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetNumIports( int numPorts )
{
   numberOfInputPorts = numPorts;
}
/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::GetIPorts(PORT& iports)
{
   for ( size_t i = 0; i <  inputPort.size(); ++i )
   {
	   
      iports[ i ] = (*inputPort.at( i ));
	  /*
      iports[ i ].GetPortLocation()->SetPoint( 
               std::pair< unsigned int, unsigned int >( poly[ 0 ].x, ( poly[ 3 ].y / inputPort.size() ) * i ) 
                                             );
      inputPort.at( i )->GetPortLocation()->SetPoint( 
               std::pair< unsigned int, unsigned int >( poly[ 0 ].x, ( poly[ 3 ].y / inputPort.size() ) * i ) 
                                            );
											*/
      //iports[ i ].x = poly[ 0 ].x;
      //iports[ i ].y = (poly[ 3 ].y / inputPort.size() ) * i;
      //iports[ i ].x = inputPort.at( i )->GetPortLocation()->GetPoint().first;
      //iports[ i ].y = inputPort.at( i )->GetPortLocation()->GetPoint().second;
   }
}
/////////////////////////////////////////////////////////////////////////////
int REI_Plugin::GetNumOports()
{
	return outputPort.size();
}
/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetNumOports( int numPorts )
{
   numberOfOutputPorts = numPorts;
}
/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::GetOPorts(PORT& oports)
{
   for ( size_t i = 0; i < outputPort.size(); ++i )
   {
      oports[ i ] = (*outputPort.at( i ));
	  /*
      oports[ i ].GetPortLocation()->SetPoint( 
               std::pair< unsigned int, unsigned int >( poly[ 1 ].x, ( poly[ 3 ].y / outputPort.size() ) * i ) 
                                             );
      outputPort.at( i )->GetPortLocation()->SetPoint( 
               std::pair< unsigned int, unsigned int >( poly[ 1 ].x, ( poly[ 3 ].y / outputPort.size() ) * i ) 
                                             );
											 */
      //oports[ i ].x = outputPort.at( i )->GetPortLocation()->GetPoint().first;
      //oports[ i ].y = outputPort.at( i )->GetPortLocation()->GetPoint().second;
   }
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::DrawIcon(wxDC* dc)
{
  //wxCoord xoff = pos.x;
  //wxCoord yoff = pos.y;
  //dc->DrawPolygon(n_pts, poly, xoff, yoff);
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::DrawID(wxDC* dc)
{
  return; // no module id
  int i;
  int x, y;
  int w, h;
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;

  wxString text;

  x=0; y=0;

  for (i=0; i<n_pts; i++)
    {
      x+=poly[i].x;
      y+=poly[i].y;
    }
  x=x/n_pts; y = y/n_pts;

  //text<<mod_pack._id;
  dc->GetTextExtent(text, &w, &h);
  dc->DrawText(text, (x-w/2+xoff), (y-h/2+yoff));
  
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::DrawName(wxDC* dc)
{
  int i;
  int x, y;
  int w, h;
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;

  x=0; y=0;

  for (i=0; i<n_pts; i++)
    {
      x+=poly[i].x;
      y+=poly[i].y;
    }
  x=x/n_pts; 
  y = y/n_pts;

  dc->GetTextExtent(name, &w, &h);
  dc->DrawText(name, (x-w/2+xoff), pos.y + (y*2));
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* REI_Plugin::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  long new_id;

  new_id = wxNewId();
  //  std::cout<<"New id "<<new_id<<std::endl;
  
  dlg = new UIDialog(parent, new_id, _("UIDialog") );

  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* REI_Plugin::Result(wxWindow* parent)
{
   std::vector<wxString> titles;
   //std::vector<std::string> descs;
   std::vector<int> alignments;

   titles.push_back( wxString("Description",wxConvUTF8) );
   alignments.push_back( wxALIGN_LEFT );
   titles.push_back( wxString("Value",wxConvUTF8));
   alignments.push_back( wxALIGN_RIGHT );

   if (result_dlg==NULL)
      result_dlg = new TextResultDialog(parent, wxT("Result Summary"), wxSize(560,400));
   result_dlg->syngas->Clear();
   result_dlg->syngas->SetColTitles( titles );
   result_dlg->syngas->SetColAlignments( alignments );
   result_dlg->Set2Cols( v_desc, v_value );
   return result_dlg;
}

/////////////////////////////////////////////////////////////////////////////
unsigned int REI_Plugin::GetID()
{
  return id;
}
/////////////////////////////////////////////////////////////////////////////
wxString REI_Plugin::GetConductorName()
{
   return wxString( "PleaseDefineConductorName",wxConvUTF8);
}
/////////////////////////////////////////////////////////////////////////////
wxString REI_Plugin::GetName()
{
   if ( name.IsEmpty() )
   {
      name = wxString("PleaseDefineClassName",wxConvUTF8);
   }

   return name;
}
/////////////////////////////////////////////////////////////////////////////
wxString REI_Plugin::GetHelp()
{
  return _T("www.vesuite.org");  
}

/////////////////////////////////////////////////////////////////////////////
wxString REI_Plugin::GetDesc()
{
  return _T("This is a default module");
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::Lock(bool lock)
{
  if (dlg!=NULL)
    dlg->Lock(lock);
}

/////////////////////////////////////////////////////////////////////////////
bool REI_Plugin::Has3Ddata()
{
  return false;
}

////////////////////////////////////////////////////////////////////
/*Interface* REI_Plugin::Pack()
{
  //string result;
  
  std::map<std::string, long *>::iterator iteri;
  std::map<std::string, double *>::iterator iterd;
  std::map<std::string, std::string *>::iterator iters;
  std::map<std::string, std::vector<long> *>::iterator itervi;
  std::map<std::string, std::vector<double> *>::iterator itervd;
  std::map<std::string, std::vector<std::string> *>::iterator itervs;


  //std::cout << "mod id : " << mod_pack._id << std::endl;
  mod_pack.setVal("XPOS",long (pos.x));
  mod_pack.setVal("YPOS",long (pos.y));
  
   for(iteri=_int.begin(); iteri!=_int.end(); iteri++)
      mod_pack.setVal(iteri->first, *(iteri->second));

   for(iterd=_double.begin(); iterd!=_double.end(); iterd++)
      mod_pack.setVal(iterd->first, *(iterd->second));

   for(iters=_string.begin(); iters!=_string.end(); iters++)
      mod_pack.setVal(iters->first, *(iters->second));

   for(itervi=_int1D.begin(); itervi!=_int1D.end(); itervi++)
      mod_pack.setVal(itervi->first, *(itervi->second));

   for(itervd=_double1D.begin(); itervd!=_double1D.end(); itervd++)
      mod_pack.setVal(itervd->first, *(itervd->second));

   for(itervs=_string1D.begin(); itervs!=_string1D.end(); itervs++)
   {
      mod_pack.setVal(itervs->first, *(itervs->second));
   }

  // EPRI TAG
  if(financial_dlg != NULL) {
    mod_pack.setVal("USE_FINANCIAL", (long)financial_dlg->_use_data);

    mod_pack.setVal("CC00", financial_dlg->_cc00_d);
    mod_pack.setVal("CC01", financial_dlg->_cc01_d);
    mod_pack.setVal("CC02", financial_dlg->_cc02_d);
    mod_pack.setVal("CC03", financial_dlg->_cc03_d);
    mod_pack.setVal("CC04", financial_dlg->_cc04_d);
    mod_pack.setVal("CC05", financial_dlg->_cc05_d);
    mod_pack.setVal("CC06", financial_dlg->_cc06_d);
    mod_pack.setVal("CC07", financial_dlg->_cc07_d);
    mod_pack.setVal("CC08", financial_dlg->_cc08_d);

    mod_pack.setVal("OM00", financial_dlg->_om00_d);
    mod_pack.setVal("OM01", financial_dlg->_om01_d);
    mod_pack.setVal("OM02", financial_dlg->_om02_d);
    mod_pack.setVal("OM03", financial_dlg->_om03_d);
  }

  //mod_pack.pack(result);
  
  //wxString wxstr = result.c_str();
 
  return &mod_pack ;//wxstr;
  
}*/
////////////////////////////////////////////////////////////////////
Model* REI_Plugin::GetModel( void )
{
   return veModel;
}
////////////////////////////////////////////////////////////////////
Model* REI_Plugin::GetVEModel( void )
{
   /*if ( veModel != NULL )
   {
      delete veModel;
   }

   veModel = new Model();
   */
   if ( name.IsEmpty() )
   {
      name = wxString("PleaseDefineClassName",wxConvUTF8);
   }
   
   veModel->SetModelName( ConvertUnicode( name.c_str() ) );
   veModel->SetModelID( id );
   veModel->SetIconFilename( iconFilename );
   veModel->GetIconLocation()->SetPoint( std::pair< unsigned int, unsigned int >( pos.x, pos.y ) );

   {
      ///Set the int data
      std::map<std::string, long *>::iterator iteri;
      for ( iteri=_int.begin(); iteri!=_int.end(); iteri++ )
      {
         Command* tempCommand = veModel->GetInput( iteri->first );
         if ( !tempCommand )
         {
            tempCommand = veModel->GetInput( -1 );
            tempCommand->SetCommandName( iteri->first );
            tempCommand->GetDataValuePair( -1 )->SetData( iteri->first, *(iteri->second) );
         }
         else
         {
            tempCommand->GetDataValuePair( 0 )->SetData( iteri->first, *(iteri->second) );
         }
      }
   }

   {
      ///Set the double data
      std::map<std::string, double *>::iterator iterd;
      for(iterd=_double.begin(); iterd!=_double.end(); iterd++)
      {
         Command* tempCommand = veModel->GetInput( iterd->first );
         if ( !tempCommand )
         {
            tempCommand = veModel->GetInput( -1 );
            tempCommand->SetCommandName( iterd->first );
            tempCommand->GetDataValuePair( -1 )->SetData( iterd->first, *(iterd->second) );
         }
         else
         {
            tempCommand->GetDataValuePair( 0 )->SetData( iterd->first, *(iterd->second) );
         }
      }
   }

   {
      ///Set the string data
      std::map<std::string, std::string *>::iterator iters;
      for(iters=_string.begin(); iters!=_string.end(); iters++)
      {
         Command* tempCommand = veModel->GetInput( iters->first );
         if ( !tempCommand )
         {
            tempCommand = veModel->GetInput( -1 );
            tempCommand->SetCommandName( iters->first );
            tempCommand->GetDataValuePair( -1 )->SetData( iters->first, *(iters->second) );
         }
         else
         {
            tempCommand->GetDataValuePair( 0 )->SetData( iters->first, *(iters->second) );
         }
      }
   }

   {
      ///Set the 1d int data
      std::map<std::string, std::vector<long> *>::iterator itervi;
      for(itervi=_int1D.begin(); itervi!=_int1D.end(); itervi++)
      {
         Command* tempCommand = veModel->GetInput( itervi->first );
         if ( !tempCommand )
         {
            tempCommand = veModel->GetInput( -1 );
            tempCommand->SetCommandName( itervi->first );
            tempCommand->GetDataValuePair( -1 )->SetData( itervi->first, *(itervi->second) );
         }
         else
         {
            tempCommand->GetDataValuePair( 0 )->SetData( itervi->first, *(itervi->second) );
         }
      }
   }

   {
      ///Set the 1d double data
      std::map<std::string, std::vector<double> *>::iterator itervd;
      for(itervd=_double1D.begin(); itervd!=_double1D.end(); itervd++)
      {
         Command* tempCommand = veModel->GetInput( itervd->first );
         if ( !tempCommand )
         {
            tempCommand = veModel->GetInput( -1 );
            tempCommand->SetCommandName( itervd->first );
            tempCommand->GetDataValuePair( -1 )->SetData( itervd->first, *(itervd->second) );
         }
         else
         {
            tempCommand->GetDataValuePair( 0 )->SetData( itervd->first, *(itervd->second) );
         }
      }
   }

   {
      ///Set the 1d string data
      std::map<std::string, std::vector<std::string> *>::iterator itervs;
      for(itervs=_string1D.begin(); itervs!=_string1D.end(); itervs++)
      {
         Command* tempCommand = veModel->GetInput( itervs->first );
         if ( !tempCommand )
         {
            tempCommand = veModel->GetInput( -1 );
            tempCommand->SetCommandName( itervs->first );
            tempCommand->GetDataValuePair( -1 )->SetData( itervs->first, *(itervs->second) );
         }
         else
         {
            tempCommand->GetDataValuePair( 0 )->SetData( itervs->first, *(itervs->second) );
         }
      }      
   }
   
   // EPRI TAG
   if ( financial_dlg != NULL ) 
   {
      Command* tempCommand = veModel->GetInput( -1 );
      tempCommand->SetCommandName( "EPRI TAG" );
      tempCommand->GetDataValuePair( -1 )->SetData( "USE_FINANCIAL", static_cast< long >( financial_dlg->_use_data ) );

      tempCommand->GetDataValuePair( -1 )->SetData( "CC00", financial_dlg->_cc00_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "CC01", financial_dlg->_cc01_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "CC02", financial_dlg->_cc02_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "CC03", financial_dlg->_cc03_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "CC04", financial_dlg->_cc04_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "CC05", financial_dlg->_cc05_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "CC06", financial_dlg->_cc06_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "CC07", financial_dlg->_cc07_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "CC08", financial_dlg->_cc08_d );

      tempCommand->GetDataValuePair( -1 )->SetData( "OM00", financial_dlg->_om00_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "OM01", financial_dlg->_om01_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "OM02", financial_dlg->_om02_d );
      tempCommand->GetDataValuePair( -1 )->SetData( "OM03", financial_dlg->_om03_d );
   }

   return veModel;
}
/////////////////////////////////////////////////////////////////////////////
/*void REI_Plugin::UnPack(Interface * intf)
{
  std::vector<std::string> vars;
  
  std::map<std::string, long *>::iterator iteri;
  std::map<std::string, double *>::iterator iterd;
  std::map<std::string, std::string *>::iterator iters;
  std::map<std::string, std::vector<long> *>::iterator itervi;
  std::map<std::string, std::vector<double> *>::iterator itervd;
  std::map<std::string, std::vector<std::string> *>::iterator itervs;
  
  unsigned int i;
  long temp;

  mod_pack = *intf;
  vars = mod_pack.getInts();
  for (i=0; i<vars.size(); i++)
    {
      iteri =_int.find(vars[i]);
      if (iteri!=_int.end())
	mod_pack.getVal(vars[i], *(iteri->second));
      else if (vars[i]=="XPOS")
	{
	  mod_pack.getVal("XPOS", temp);
	  //	  std::cout << "xpos " << temp << std::endl;
	  pos.x = temp;
	}
      else if (vars[i]=="YPOS")
	{
	  //	  std::cout << "ypos " << temp << std::endl;
	  mod_pack.getVal("YPOS", temp);
	  pos.y = temp;
	}
    }

  vars = mod_pack.getDoubles();
  for (i=0; i<vars.size(); i++)
    {
      iterd =_double.find(vars[i]);
      if (iterd!=_double.end())
	mod_pack.getVal(vars[i], *(iterd->second));
    }  
  
  vars = mod_pack.getStrings();
  for (i=0; i<vars.size(); i++)
    {
      iters =_string.find(vars[i]);
      if (iters!=_string.end())
	mod_pack.getVal(vars[i], *(iters->second));
    }

  vars = mod_pack.getInts1D();
  for (i=0; i<vars.size(); i++)
    {
      itervi =_int1D.find(vars[i]);
      if (itervi!=_int1D.end())
	mod_pack.getVal(vars[i], *(itervi->second));
    }

  vars = mod_pack.getDoubles1D();
  for (i=0; i<vars.size(); i++)
    {
      itervd =_double1D.find(vars[i]);
      if (itervd!=_double1D.end())
	mod_pack.getVal(vars[i], *(itervd->second));
    }

   vars = mod_pack.getStrings1D();
   for (i=0; i<vars.size(); i++)
   {
      itervs =_string1D.find(vars[i]);
      if (itervs!=_string1D.end())
      {
         //unsigned int testInt = (*itervs->second).size();
         //unsigned int idint = this->GetID();
         std::vector<std::string> tempvector = (*itervs->second);
         mod_pack.getVal(vars[i], tempvector );
         (*itervs->second) = tempvector;
      }
   }

  // EPRI TAG
  long uf = 0;
  if(mod_pack.getVal("USE_FINANCIAL", uf)) {
    
    if(financial_dlg == NULL)
      financial_dlg = new FinancialDialog (NULL, (wxWindowID)-1);
    
    financial_dlg->_use_data = uf;
    
    financial_dlg->_cc00_d = mod_pack.getDouble("CC00");
    financial_dlg->_cc01_d = mod_pack.getDouble("CC01");
    financial_dlg->_cc02_d = mod_pack.getDouble("CC02");
    financial_dlg->_cc03_d = mod_pack.getDouble("CC03");
    financial_dlg->_cc04_d = mod_pack.getDouble("CC04");
    financial_dlg->_cc05_d = mod_pack.getDouble("CC05");
    financial_dlg->_cc06_d = mod_pack.getDouble("CC06");
    financial_dlg->_cc07_d = mod_pack.getDouble("CC07");
    financial_dlg->_cc08_d = mod_pack.getDouble("CC08");

    financial_dlg->_om00_d = mod_pack.getDouble("OM00");
    financial_dlg->_om01_d = mod_pack.getDouble("OM01");
    financial_dlg->_om02_d = mod_pack.getDouble("OM02");
    financial_dlg->_om03_d = mod_pack.getDouble("OM03");
  }
}*/
/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetVEModel( VE_XML::VE_Model::Model* tempModel )
{
   if ( veModel != NULL )
   {
      delete veModel;
   }

   veModel = tempModel;

   //veModel->SetObjectFromXMLData( modelElement );
   name = wxString( veModel->GetModelName().c_str(),wxConvUTF8 );
   id = veModel->GetModelID();
   std::string tempFilename = veModel->GetIconFilename();
   pos.x = veModel->GetIconLocation()->GetPoint().first;
   pos.y = veModel->GetIconLocation()->GetPoint().second;

   unsigned int numInputs = veModel->GetNumberOfInputs();
   for ( unsigned int i = 0; i < numInputs; ++i )
   {
      Command* commandData = veModel->GetInput( i );
      // Add if statement for input variables
      //if "EPRI TAG"
      //else
      {
         for ( unsigned int k = 0; k < commandData->GetNumberOfDataValuePairs(); ++k )
         {
            DataValuePair* tempData = commandData->GetDataValuePair( k );
            std::string dataName = tempData->GetDataName();
            std::string dataType = tempData->GetDataType();
            // to grab the data from the maps properly            
            std::map<std::string, long *>::iterator iteri;
            std::map<std::string, double *>::iterator iterd;
            std::map<std::string, std::string *>::iterator iters;
            std::map<std::string, std::vector<long> *>::iterator itervi;
            std::map<std::string, std::vector<double> *>::iterator itervd;
            std::map<std::string, std::vector<std::string> *>::iterator itervs;

            if ( std::string( "FLOAT" ) == dataType )
            {
               iterd = _double.find( dataName );
               if ( iterd != _double.end() )
                  tempData->GetData( *(iterd->second) );
            }
            else if ( std::string( "LONG" ) == dataType )
            {
               iteri = _int.find( dataName );
               if ( iteri != _int.end() )
               {   
                  tempData->GetData( *(iteri->second) );
               }
            }
            else if ( std::string( "STRING" ) == dataType )
            {
               iters = _string.find( dataName );
               if ( iters != _string.end() )
                  tempData->GetData( *(iters->second) );
            }
            else if ( std::string( "1DSTRING" ) == dataType )
            {
               itervs = _string1D.find( dataName );
               if ( itervs != _string1D.end() )
                  tempData->GetData( *(itervs->second) );
            }
            else if ( std::string( "1DDOUBLE" ) == dataType )
            {
               itervd = _double1D.find( dataName );
               if ( itervd != _double1D.end() )
                  tempData->GetData( *(itervd->second) );
            }
            else if ( std::string( "1DLONG" ) == dataType )
            {
               itervi = _int1D.find( dataName );
               if ( itervi != _int1D.end() )
                  tempData->GetData( *(itervi->second) );
            }
            /*else if ( std::string( "XMLOBJECT" ) == dataType )
            {
               iteri = _double.find( dataName );
               if ( iteri != _double.end() );
               tempData->GetData( *(iteri->second) );
               tempData->GetData( *(_int1D[ dataName ]) );
            }*/
         }
      }
      /*
        // EPRI TAG
        long uf = 0;
        if(mod_pack.getVal("USE_FINANCIAL", uf)) {

          if(financial_dlg == NULL)
            financial_dlg = new FinancialDialog (NULL, (wxWindowID)-1);

          financial_dlg->_use_data = uf;

          financial_dlg->_cc00_d = mod_pack.getDouble("CC00");
          financial_dlg->_cc01_d = mod_pack.getDouble("CC01");
          financial_dlg->_cc02_d = mod_pack.getDouble("CC02");
          financial_dlg->_cc03_d = mod_pack.getDouble("CC03");
          financial_dlg->_cc04_d = mod_pack.getDouble("CC04");
          financial_dlg->_cc05_d = mod_pack.getDouble("CC05");
          financial_dlg->_cc06_d = mod_pack.getDouble("CC06");
          financial_dlg->_cc07_d = mod_pack.getDouble("CC07");
          financial_dlg->_cc08_d = mod_pack.getDouble("CC08");

          financial_dlg->_om00_d = mod_pack.getDouble("OM00");
          financial_dlg->_om01_d = mod_pack.getDouble("OM01");
          financial_dlg->_om02_d = mod_pack.getDouble("OM02");
          financial_dlg->_om03_d = mod_pack.getDouble("OM03");
        }
      */
   }

   //Setup the ports so that the plugin can access them.
   for ( size_t i = 0; i < veModel->GetNumberOfPorts(); ++i )
   {
      VE_XML::VE_Model::Port* tempPort = veModel->GetPort( i );
      if ( tempPort->GetDataFlowDirection() == std::string( "input" ) )
      {
         inputPort.push_back( tempPort );
      }
      else if ( tempPort->GetDataFlowDirection() == std::string( "output" ) )
      {
         outputPort.push_back( tempPort );
      }
      else
      {
         wxMessageDialog( NULL, _("Improperly formated ves file."), 
                  _("VES File Read Error"), wxOK | wxICON_ERROR, wxDefaultPosition );
      }
   }

   //
}
/////////////////////////////////////////////////////////////////////////////
/*void REI_Plugin::UnPackResult(Interface* intf)
{
   //This will be module dependent. 
   //here is the Default implementation when using the 
   //summary table to pack things up in the module end
  
   std::vector<std::string> descs;
   descs = intf->getStrings();
   v_desc.clear();
   v_value.clear();
   for ( unsigned int i=0; i<descs.size(); ++i )
   {
      std::string desc = descs[i];
      std::string value = intf->getString(descs[i]);
      if (desc.substr(0,3) == "***") 
         desc = desc.substr( 9, desc.size()-9 );
     
      v_desc.push_back ( wxString(desc.c_str(),wxConvUTF8));
      v_value.push_back( wxString(value.c_str(),wxConvUTF8));
   }
}*/
/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::RegistVar(std::string vname, long *var)
{
  _int[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, double *var)
{
  _double[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, std::string *var)
{
  _string[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, std::vector<long> *var)
{
  _int1D[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, std::vector<double> *var)
{
  _double1D[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, std::vector<std::string> *var)
{
  _string1D[vname]=var;
}

///////////////////////////////////////////////
/*UIDialog* REI_Plugin::PortData(wxWindow* parent,  Interface *it)
{
  //This default implementation is for the Gas and water Interface's Data package
  //New modules can override the function to implement its own port dialog
  std::vector<wxString> titles;
  titles.push_back(wxString("Description",wxConvUTF8));
  titles.push_back(wxString("Value",wxConvUTF8));

  std::vector<wxString> gas_desc;
  std::vector<wxString> gas_value;
  unsigned int i;
  //first, to decide if it is water or gas

   bool ok = true;
   it->getDouble("ENTHALPY",    &ok);
   if (!ok)
   { //gas
      gas_desc.push_back(wxString("COALCAL",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("COALCAL")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("ASHCAL",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("ASHCAL")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("ASHPH",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("ASHPH")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("PRESSURE_DROP",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("PRESSURE_DROP")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("TEMPERATURE",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("TEMPERATURE")).c_str(),wxConvUTF8));
	   gas_desc.push_back(wxString("MW",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("MW")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("PRESSURE",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("PRESSURE")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("FLOWRATE",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("FLOWRATE")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("TAR",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("TAR")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("SOOT",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("SOOT")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("PT MEAN_SIZE",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("MEAN_SIZE")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("PT SIZE_VARIANCE",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("SIZE_VARIANCE")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("PARTICLE T",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("T_PARTICLE")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("PARITCLE M",wxConvUTF8));
      gas_value.push_back(wxString(to_string(it->getDouble("M_PARTICLE")).c_str(),wxConvUTF8));
      gas_desc.push_back(wxString("COMP NAME",wxConvUTF8));
      gas_value.push_back(wxString("COMP CONC",wxConvUTF8));
      
      std::vector<std::string> comp_name = it->getString1D("COMP_NAME");
      std::vector<double>      comp_conc = it->getDouble1D("COMP_CONC");
      for (i=0; i<comp_name.size(); i++)
	   {
	      gas_desc.push_back( wxString(comp_name[i].c_str(),wxConvUTF8));
	      gas_value.push_back( wxString(to_string(comp_conc[i]).c_str(),wxConvUTF8));
	   }
      
      gas_desc.push_back( wxString("PART NAME",wxConvUTF8));
      gas_value.push_back( wxString("PART CONC",wxConvUTF8));

      std::vector<std::string> part_name = it->getString1D("PART_NAME");
      std::vector<double>      part_conc = it->getDouble1D("PART_CONC");
      for (i=0; i<part_name.size(); i++)
	   {
	      gas_desc.push_back( wxString(part_name[i].c_str(),wxConvUTF8));
	      gas_value.push_back( wxString(to_string(part_conc[i]).c_str(),wxConvUTF8));
	   }
      
      gas_desc.push_back( wxString("WIC NAME",wxConvUTF8));
      gas_value.push_back( wxString("WIC CONC",wxConvUTF8));
      std::vector<std::string> wic_name = it->getString1D("WIC_NAME", &ok);
      std::vector<double>      wic_conc = it->getDouble1D("WIC_CONC", &ok);
      for (i=0; i<wic_name.size(); i++)
	   {
	      gas_desc.push_back( wxString(wic_name[i].c_str(),wxConvUTF8));
	      gas_value.push_back( wxString(to_string(wic_conc[i]).c_str(),wxConvUTF8));
	   }
   }
   else
   { //water
      gas_desc.push_back( wxString("TEMPERATURE",wxConvUTF8) );
      gas_value.push_back( wxString( to_string(it->getDouble("TEMPERATURE")).c_str(), wxConvUTF8) );
      gas_desc.push_back( wxString("PRESSURE", wxConvUTF8) );
      gas_value.push_back( wxString( to_string(it->getDouble("PRESSURE")).c_str(), wxConvUTF8) );
      gas_desc.push_back( wxString("ENTHALPY", wxConvUTF8) );
      gas_value.push_back( wxString( to_string(it->getDouble("ENTHALPY")).c_str(), wxConvUTF8) );
      gas_desc.push_back(wxString("QUALITY", wxConvUTF8) );
      gas_value.push_back( wxString( to_string(it->getDouble("QUALITY")).c_str(), wxConvUTF8 ) );
      gas_desc.push_back( wxString("FLOWRATE", wxConvUTF8) );
      gas_value.push_back( wxString( to_string(it->getDouble("FLOWRATE")).c_str(), wxConvUTF8 ) );
    
   }

   if (port_dlg==NULL)
      port_dlg = new TextResultDialog(parent);
  
   port_dlg->syngas->Clear();
   port_dlg->syngas->AddRow(titles);
   port_dlg->syngas->AddSeperator(' ');
   port_dlg->syngas->AddSeperator('+');
   port_dlg->syngas->AddSeperator(' ');
   port_dlg->Set2Cols(gas_desc, gas_value);

   return port_dlg;
}*/
// EPRI TAG
///////////////////////////////////////////////
void REI_Plugin::FinancialData ()
{
  if(financial_dlg == NULL)
    financial_dlg = new FinancialDialog (NULL, (wxWindowID)-1);
  
  financial_dlg->Show();
}
///////////////////////////////////////////////
void REI_Plugin::ViewInputVariables( void )
{
   if ( inputsDialog )
   {
      inputsDialog->Destroy();
      inputsDialog = 0;
   }

   inputsDialog = new SummaryResultDialog(NULL, wxT("Input Variables"), wxSize(560, 400));
   // Get all the inputs form the model
   for ( size_t i = 0; i < veModel->GetNumberOfInputs(); ++i )
   {
      std::vector< wxString > tagNames;
      std::vector< wxString > values;
      VE_XML::Command* inputCommand = veModel->GetInput( i );
      GetDataTables( inputCommand, tagNames, values );
      std::string inputParamter = inputCommand->GetCommandName();
      inputsDialog->NewTab( wxString( inputParamter.c_str(), wxConvUTF8 ) );
      inputsDialog->Set2Cols( tagNames, values );
   }
   // Get all the results form the model
   inputsDialog->Show();
}
///////////////////////////////////////////////
void REI_Plugin::ViewResultsVariables( void )
{
   if ( resultsDialog )
   {
      resultsDialog->Destroy();
      resultsDialog = 0;
   }
   
   resultsDialog = new SummaryResultDialog(NULL, wxT("Results Variables"), wxSize(560, 400));
   // Get all the inputs form the model
   for ( size_t i = 0; i < veModel->GetNumberOfResults(); ++i )
   {
      std::vector< wxString > tagNames;
      std::vector< wxString > values;
      VE_XML::Command* inputCommand = veModel->GetResult( i );
      GetDataTables( inputCommand, tagNames, values );
      std::string inputParamter = inputCommand->GetCommandName();
      resultsDialog->NewTab( wxString( inputParamter.c_str(), wxConvUTF8 ) );
      resultsDialog->Set2Cols( tagNames, values );
   }
   resultsDialog->Show();
}
///////////////////////////////////////////////
void REI_Plugin::GetDataTables( VE_XML::Command* inputCommand, std::vector< wxString >& tagNames, std::vector< wxString >& values )
{
   for ( size_t j = 0; j < inputCommand->GetNumberOfDataValuePairs(); ++j )
   {
      VE_XML::DataValuePair* tempDVP = inputCommand->GetDataValuePair( j );
      std::string dataType = tempDVP->GetDataType();
      std::string dataName = tempDVP->GetDataName();
      std::string stringData = "empty";

      if ( dataType == std::string("FLOAT") )
      {
         double doubleData;
         tempDVP->GetData( doubleData );
         stringData = ::to_string( doubleData );
      }
      else if ( dataType == std::string("UNSIGNED INT") )
      {
         unsigned int intData;
         tempDVP->GetData( intData );
         stringData = ::to_string( intData );
      }
      else if ( dataType == std::string("LONG") )
      {
         long longData;
         tempDVP->GetData( longData );
         stringData = ::to_string( static_cast< int >( longData ) );
      }
      else if ( dataType == std::string("STRING") )
      {
         tempDVP->GetData( stringData );
      }
      // vectors of data to be displayed
      tagNames.push_back( wxString( dataName.c_str(), wxConvUTF8 ) );
      values.push_back( wxString( stringData.c_str(), wxConvUTF8 ) );
   }
}
////////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetPluginNameDialog( void )
{
   wxTextEntryDialog newPluginName( 0, 
                                    _("Enter the name for your UI plugin:"),
                                    _("Set UI Plugin Name..."),
                                    _("YourPluginName"),wxOK|wxCANCEL);

   if ( newPluginName.ShowModal() == wxID_OK )
   {
      name = newPluginName.GetValue(); 
   }
}
////////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetCORBAService( VE_Conductor::CORBAServiceList* serviceList )
{
   this->serviceList = serviceList;
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetImageIcon(std::string path, float rotation, int mirror, float scale)
{
std::cout << "icon path " << path << std::endl;
   //Try and find default icons if needed
   std::map< std::string, wxImage >::iterator iter = defaultIconMap.find( path );
   if ( iter != defaultIconMap.end() )
   {
      iconFilename = path;
      //now scale it up or down according to the specified scale
      icon_w = iter->second.GetWidth();
      icon_h = iter->second.GetHeight();
      
      delete my_icon;
      my_icon = new wxBitmap( iter->second );

      n_pts = 4;
      
      poly[0]=wxPoint(0,0);
      poly[1]=wxPoint(icon_w,0);
      poly[2]=wxPoint(icon_w,icon_h);
      poly[3]=wxPoint(0,icon_h);
      return;
   }
	//wxImage* my_img = new wxImage();
	//bool exists = my_img->LoadFile(wxString(path.c_str(),wxConvUTF8), wxBITMAP_TYPE_JPEG);
	std::string fullPath = "2DIcons/" + path + ".jpg";
	std::ifstream exists(fullPath.c_str());
	double PI = 3.14159265;
	if ( exists.fail() )
	{	
      return;
   }
   iconFilename = path;
	wxImage image(wxString(fullPath.c_str(),wxConvUTF8), wxBITMAP_TYPE_JPEG);
	if(mirror > 0 && mirror < 3)
	{
		if(mirror == 1)
			image = image.Mirror(true);
		else
			image = image.Mirror(false);
	}
	image = image.Rotate((rotation*PI)/180, wxPoint(0,0));

	//Implement Scale - scale the images to where the longest length is 40
	//while the smallest length is scaled accordingly
	
	if(image.GetWidth() > image.GetHeight())
	{
		icon_w = 40;
		icon_h = 40 * image.GetHeight() / image.GetWidth();
	}
	else
	{
		icon_h = 40;
		icon_w = 40 * image.GetWidth() / image.GetHeight();
	}

	//now scale it up or down according to the specified scale
	icon_w = icon_w * scale;
	icon_h = icon_h * scale;
	
	//int icon_h = 40;
	//int icon_w = 40;

	delete my_icon;
	my_icon=new wxBitmap(image.Scale(icon_w, icon_h));
	
	
	n_pts = 4;

	poly[0]=wxPoint(0,0);
	poly[1]=wxPoint(icon_w,0);
	poly[2]=wxPoint(icon_w,icon_h);
	poly[3]=wxPoint(0,icon_h);
}
