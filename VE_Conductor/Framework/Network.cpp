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
 * File:          $RCSfile: Network.cpp Network.cppNetwork.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/Network.h"
#include "VE_Conductor/GUIPlugin/PortDialog.h"
#include "VE_Conductor/Network/package.h"
#include "VE_Conductor/Framework/paraThread.h"
#include "VE_Conductor/GUIPlugin/Geometry.h"
#include "VE_Conductor/GUIPlugin/UIDialog.h"
#include "VE_Conductor/GUIPlugin/GlobalParamDialog.h"
#include "VE_Conductor/Framework/Frame.h"
#include "VE_Conductor/Framework/App.h"
#include "VE_Conductor/Framework/CORBAServiceList.h"

#include "VE_Open/XML/Model/Network.h"
#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Model.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/XMLReaderWriter.h"

#include "VE_Open/XML/CAD/CADAssembly.h"

#include "VE_Conductor/Utilities/CADNodeManagerDlg.h"
#include "VE_Conductor/Framework/DataSetLoaderUI.h"
#include "VE_Conductor/Framework/vistab.h"

#include <wx/dc.h>
#include <wx/dcbuffer.h>
#include <wx/utils.h>
#include <wx/progdlg.h>

#include <sstream>
#include <iomanip>
#include <iostream>
#include <cmath>

using namespace VE_Conductor::GUI_Utilities;

BEGIN_EVENT_TABLE(Network, wxScrolledWindow)
   // see the docs on wxScrolledWindow for more info on this
   // Also see wxPaintEvent
   // overriding this function allows us to handle when things on redrawn
   EVT_PAINT(Network::OnPaint)
   //See wxMoveEvent for info on this
   // This process info whenever the mouse moves on the design canvas
   EVT_MOTION(Network::OnMouseMove)
   //
   EVT_LEFT_DOWN(Network::OnMLeftDown)
   EVT_LEFT_UP(Network::OnMLeftUp)
   // bring up custom ui dialog
   EVT_LEFT_DCLICK(Network::OnDClick)
   // brings up the design canvas menu on a specfic module
   EVT_RIGHT_DOWN(Network::OnMRightDown)
   EVT_MENU(ADD_TAG, Network::OnAddTag)
   EVT_MENU(ADD_LINK_CON, Network::OnAddLinkCon)
   EVT_MENU(EDIT_TAG, Network::OnEditTag)
   EVT_MENU(DEL_TAG, Network::OnDelTag)
   EVT_MENU(DEL_LINK, Network::OnDelLink)
   EVT_MENU(DEL_LINK_CON, Network::OnDelLinkCon)
   EVT_MENU(DEL_MOD, Network::OnDelMod)
   EVT_MENU(SHOW_LINK_CONT, Network::OnShowLinkContent)
   // The following are all results of right click and chossing from the 
   // menu that is displayed with right click
   EVT_MENU(SHOW_RESULT, Network::OnShowResult)
   EVT_MENU(PARAVIEW, Network::OnParaView)
   EVT_MENU(SHOW_DESC, Network::OnShowDesc)
   EVT_MENU(MODEL_INPUTS, Network::OnInputsWindow) /* EPRI TAG */
   EVT_MENU(SHOW_FINANCIAL, Network::OnShowFinancial) /* EPRI TAG */
   EVT_MENU(GEOMETRY, Network::OnGeometry)
   EVT_MENU(DATASET, Network::OnDataSet)
   EVT_MENU(MODEL_INPUTS, Network::OnInputsWindow) /* EPRI TAG */
   EVT_MENU(MODEL_RESULTS, Network::OnResultsWindow) /* EPRI TAG */
   EVT_MENU(VISUALIZATION, Network::OnVisualization)
END_EVENT_TABLE()

Network::Network(wxWindow* parent, int id)
  :wxScrolledWindow(parent, id, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL | wxNO_FULL_REPAINT_ON_RESIZE)
{
   modules.clear();
   links.clear();
   userScale.first=1;
   userScale.second=1;
   GetNumUnit()->first=240;
   GetNumUnit()->second=240;
   GetNumPix()->first = 10;
   GetNumPix()->second = 10;
   SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
   m_selMod = -1;
   m_selFrPort = -1; 
   m_selToPort = -1; 
   m_selLink = -1; 
   m_selLinkCon = -1; 
   m_selTag = -1; 
   m_selTagCon = -1; 
   xold = yold =0;
   moving = false;
   paraview = false;
   globalparam_dlg = new GlobalParamDialog(NULL, -1);
   veNetwork = 0;
   isLoading = false;
   cadDialog = 0;
   SetBackgroundColour(*wxWHITE);
   this->parent = parent;
   vistab = 0;
}

Network::~Network()
{
   links.clear();

   if( cadDialog)
   {
      cadDialog->Destroy();
      cadDialog = 0;
   }
   if(vistab)
   {
      vistab->Destroy();
      vistab = 0;
   }

   /*for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      delete modules[ iter->first ];
   }*/
   modules.clear();
   delete globalparam_dlg;
}
/////////////////////////////////////////////
///////// Event Handlers ////////////////////
/////////////////////////////////////////////

void Network::OnPaint(wxPaintEvent& WXUNUSED( event ) )
{
   while ( (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR) ) { ; }

   wxPaintDC dc(this);
   PrepareDC(dc);
  
   dc.SetUserScale( userScale.first, userScale.second );
   // dc.Clear();
  
   ReDraw(dc); 

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
}

/////////////////////////////////////////////////////////////
void Network::OnMLeftDown(wxMouseEvent& event)
{
  wxRect bbox;
  wxPoint pos, temp;
  std::map< int, Module >::iterator iter;
  PORT ports;
 	
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( userScale.first, userScale.second );

  wxPoint evtpos = event.GetLogicalPosition(dc);
 
   //First, check if any module is selected
   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      int i;
      i=iter->first;
      temp.x = evtpos.x;
      temp.y = evtpos.y;
      if ( modules[i].GetPolygon()->inside( temp ) )
	   {
	      m_selMod = i;
	      bbox = modules[i].GetPlugin()->GetBBox();
	      relative_pt.x = temp.x - bbox.x;
	      relative_pt.y = temp.y - bbox.y;
	      break;
	   }
   }

   //Second, check if selected module's Iports or Oports is selected
   if (m_selMod>=0)
   {
      bbox = modules[m_selMod].GetPlugin()->GetBBox();
      pos.x = bbox.x;
      pos.y = bbox.y;
      
      temp.x = evtpos.x - pos.x;
      temp.y = evtpos.y - pos.y;

      ports.resize( modules[m_selMod].GetPlugin()->GetNumIports() );
      modules[m_selMod].GetPlugin()->GetIPorts( ports );
      for ( unsigned int i=0; i<ports.size(); i++)
      {
         wxPoint tempPoint( ports[i].GetPortLocation()->GetPoint().first, ports[i].GetPortLocation()->GetPoint().second );
	      if (computenorm(temp, tempPoint)<=10)
	      {
	         m_selFrPort = i;
	         break;
	      }
      }

      ports.resize( modules[m_selMod].GetPlugin()->GetNumOports() );
	   modules[m_selMod].GetPlugin()->GetOPorts( ports );
      for ( unsigned int i=0; i<ports.size(); i++)
	   {
         wxPoint tempPoint( ports[i].GetPortLocation()->GetPoint().first, ports[i].GetPortLocation()->GetPoint().second );
         if ( computenorm( temp, tempPoint )<=10)
	      {
	         m_selToPort = i;
	         break;
	      }
      }
   }
   //Third, check if any link connector is selected

   if (m_selLink>=0)
   {
      for (unsigned int i=0; i<links[m_selLink].GetPoints()->size(); i++)
	      if (computenorm( evtpos, *(links[m_selLink].GetPoint( i )) )<=3)
	      {
	         m_selLinkCon=i;
	         break;
	      }
   }

   //Forth, check if any tag is selected
   for ( unsigned int i=0; i<tags.size(); i++)
   {
      if ( tags[i].GetBoundingBox()->Inside(evtpos) )
	   {
	      m_selTag=i;
	      tag_rpt.x = evtpos.x - tags[i].GetBoundingBox()->x;
	      tag_rpt.y = evtpos.y - tags[i].GetBoundingBox()->y;
	      break;
	   }
   }

   //At last, check if any tag connector is selected
   for (unsigned int i=0; i<tags.size(); i++)
   {
      if (computenorm( evtpos, *(tags[i].GetConnectorsPoint( 0 )) )<=3)
	   {
	      m_selTag=i;
	      m_selTagCon=0;
	      break;
	   }
      if (computenorm(evtpos, *(tags[i].GetConnectorsPoint( 1 )) ) <=3)
	   {
	      m_selTag=i;
	      m_selTagCon=1;
	      break;
	   }
   }
}

////////////////////////////////////////////////////////////////////
void Network::OnMouseMove(wxMouseEvent& event)
{
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( userScale.first, userScale.second );
  
  wxPoint evtpos = event.GetLogicalPosition(dc);
  
  long x = evtpos.x;
  long y = evtpos.y;

   if (!event.Dragging())
   {
      // if we were dragging a module around
	   if (moving)
	   {
		   OnMLeftUp(event);
		   Refresh();
	   }
	  
      if (m_selMod>=0 && SelectMod(x, y)<0 ) 
	  		UnSelectMod(dc); //Unselect only get called by state changed from something selected state to nothing selected state
	   else if (m_selLink>=0 && SelectLink(x, y )<0)
	      UnSelectLink(dc);
      else if (m_selTag>=0 && SelectTag(x, y)<0)
	      UnSelectTag(dc);
      else
	   {
	      // To avoid the shortcut evaluation
	      SelectMod(x, y);
	      SelectLink(x, y );
	      SelectTag(x, y); 
	      //Check if link con is selected
	      m_selLinkCon = -1;
	      if (m_selLink>=0)
	      {
	         for ( unsigned int i=0; i<links[m_selLink].GetPoints()->size(); i++)
		         if ( computenorm( evtpos, *(links[m_selLink].GetPoint( i )) ) <= 3 )
		         {
		            m_selLinkCon=i;
		            break;
		         }
	      }
	 
	      //Check if tag con is selected
	      m_selTagCon = -1;
	      if (m_selTag>=0)
	      {
	         if (computenorm( evtpos, *(tags[m_selTag].GetConnectorsPoint( 0 )) ) <= 3 )
		         m_selTagCon=0;
	         if (computenorm( evtpos, *(tags[m_selTag].GetConnectorsPoint( 1 )) ) <= 3 )
		         m_selTagCon=1;
	      }
	   }
   }
   else //dragging
   {
	   moving = true; 
      if (m_selLinkCon>=0 && m_selLink>=0)
	   {
         MoveLinkCon(x, y, m_selLink, m_selLinkCon, dc);
      }
      else if (m_selTag>=0 && m_selTagCon<0)
	   {
         MoveTag(x, y, m_selTag, dc);
      }
      else if (m_selTag>=0 && m_selTagCon>=0)
	   {
         MoveTagCon(x, y, m_selTag, m_selTagCon, dc);
      }
      else if (m_selMod>=0 && m_selFrPort>=0)
	   {
         TryLink(x, y, m_selMod, m_selFrPort, dc, true); // draw input ports
      }
      else if (m_selMod>=0 && m_selToPort>=0)
	   {
         TryLink(x, y, m_selMod, m_selToPort, dc, false); // draw output ports
      }
      else if (m_selMod>=0 && m_selFrPort<0 && m_selToPort<0)
	   {
         MoveModule(x, y, m_selMod, dc);
      }
   }
}

/////////////////////////////////////////////////////////////////////
void Network::OnMLeftUp(wxMouseEvent& event)
{
   wxClientDC dc(this);
   PrepareDC(dc);
   dc.SetUserScale( userScale.first, userScale.second );

   wxPoint evtpos = event.GetPosition();
   long x = dc.DeviceToLogicalX( evtpos.x );
   long y = dc.DeviceToLogicalY( evtpos.y );
  
   if (m_selLinkCon>=0 && m_selLink>=0)
   {
      // We will create the link connector (basically a bend point)
      DropLinkCon(x, y, m_selLink, m_selLinkCon, dc);
      m_selLinkCon = -1;
      m_selLink=-1;
   }
   else if (m_selTag>=0 && m_selTagCon<0)
   {
      // drop the tag we just created
      DropTag(x, y, m_selTag, dc);
      m_selTag=-1;
   }
   else if (m_selTag>=0 && m_selTagCon>=0)
   {
      // We will create the tag connector (basically a bend point)
      DropTagCon(x, y, m_selTag, m_selTagCon, dc);
      m_selTag=-1;
      m_selTagCon=-1;
   }
   else if (m_selMod>=0 && m_selFrPort>=0)
   {
      // drop the start point of the link
      DropLink(x, y, m_selMod, m_selFrPort, dc, true);
      m_selMod = -1;
      m_selFrPort = -1;
   }
   else if (m_selMod>=0 && m_selToPort>=0)
   {
      // drop the final point of the link
      DropLink(x, y, m_selMod, m_selToPort, dc, false);
      m_selMod = -1;
      m_selToPort = -1;
   }
   else if (m_selMod>=0 && m_selFrPort<0 && m_selToPort<0)
   {
      //drop a module after dragging it around
      DropModule(x, y, m_selMod );
      m_selMod = -1;
   }
   moving = false;
}

////////////////////////////////////////////////////////////////
void Network::OnDClick( wxMouseEvent& event )
{
   // This function opens a plugins dialog when double clicked on the design canvas
   wxClientDC dc( this );
   PrepareDC( dc );
   dc.SetUserScale( userScale.first, userScale.second );

   wxPoint evtpos = event.GetLogicalPosition( dc );

   // set the m_selMod class variable
   SelectMod( evtpos.x, evtpos.y );

   // now use it
   if ( m_selMod >= 0 )
   {
      // now show the custom dialog with no parent for the wxDialog
      UIDialog* hello = modules[m_selMod].GetPlugin()->UI( NULL );
      if ( hello!=NULL )
      {
         hello->Show();
      }
      m_selMod = -1;
   }
}

///////////////////////////////////////////////////////////////
void Network::OnMRightDown(wxMouseEvent& event)
{
   wxClientDC dc(this);
   PrepareDC(dc);
   dc.SetUserScale( userScale.first, userScale.second );

   wxMenu pop_menu(_T("Action"));

   pop_menu.Append(ADD_TAG, _T("Add Tag")); //This will always be enable

   pop_menu.Append(ADD_LINK_CON, _T("Add Link Connector"));
   pop_menu.Append(EDIT_TAG, _T("Edit Tag"));
   pop_menu.Append(DEL_LINK_CON, _T("Delete Link Connector"));
   pop_menu.Append(DEL_LINK, _T("Delete Link"));
   pop_menu.Append(DEL_TAG, _T("Delete Tag"));
   pop_menu.Append(DEL_MOD, "Del Module");

   pop_menu.Append(SHOW_DESC, "Show Module Description");	
   pop_menu.Append(SHOW_RESULT, "Show Module Result");
   pop_menu.Append(PARAVIEW, "ParaView 3D Result");

   pop_menu.Append(SHOW_LINK_CONT, "Show Link Content");

   // EPRI TAG
   AppFrame* p_frame;
   p_frame = ((AppFrame*)(parent->GetParent()->GetParent()));
   if (p_frame->f_financial)
   {
	pop_menu.Append(SHOW_FINANCIAL, "Financial Data");
	pop_menu.Enable(SHOW_FINANCIAL, true);
   }

   //if (p_frame->f_geometry)
   {
	// GUI to configure geometry for graphical env
	pop_menu.Append(GEOMETRY, "Geometry Config");
	pop_menu.Enable(GEOMETRY, true);
   // GUI to configure dataset for graphical env
   pop_menu.Append(DATASET, "Data Set Config");
   pop_menu.Enable(DATASET, true);

	// GUI to configure geometry for graphical env
   }
	
   pop_menu.Append(MODEL_INPUTS, "Input Variables" );
   pop_menu.Enable(MODEL_INPUTS, true);
   pop_menu.Append(MODEL_RESULTS, "Result Variables" );
   pop_menu.Enable(MODEL_RESULTS, true);

   pop_menu.Append(VISUALIZATION, "Visualization" );
   pop_menu.Enable(VISUALIZATION, true);

   pop_menu.Enable(ADD_LINK_CON, false);
   pop_menu.Enable(EDIT_TAG, false);
   pop_menu.Enable(DEL_LINK_CON, false);
   pop_menu.Enable(DEL_LINK, false);
   pop_menu.Enable(DEL_TAG, false);
   pop_menu.Enable(DEL_MOD, false);
   pop_menu.Enable(SHOW_RESULT, false);
   pop_menu.Enable(PARAVIEW, false);

   pop_menu.Enable(SHOW_LINK_CONT, false);

   if (m_selLink>=0)
   {
      pop_menu.Enable(DEL_LINK, true);
      pop_menu.Enable(SHOW_LINK_CONT, true);
      if (m_selLinkCon>=0) 
         pop_menu.Enable(DEL_LINK_CON, true);
      else
         pop_menu.Enable(ADD_LINK_CON, true);
   }

   if (m_selTag>=0 )
   {
      pop_menu.Enable(EDIT_TAG, true);
      pop_menu.Enable(DEL_TAG, true);
   }

   if (m_selMod>=0)
   {
      pop_menu.Enable(DEL_MOD, true);
      pop_menu.Enable(SHOW_RESULT, true);
      if (modules[m_selMod].GetPlugin()->Has3Ddata())
         pop_menu.Enable(PARAVIEW, true);
   }

   action_point = event.GetLogicalPosition(dc);
   PopupMenu(&pop_menu, event.GetPosition());


   m_selMod = -1;
   m_selFrPort = -1; 
   m_selToPort = -1; 
   m_selLink = -1; 
   m_selLinkCon = -1; 
   m_selTag = -1; 
   m_selTagCon = -1; 
   xold = yold =0;
}

//////// Menu event handlers ////////////////////////

void Network::OnAddTag(wxCommandEvent& WXUNUSED(event))
{
   wxTextEntryDialog dialog(this,_T("Tag Editor"), _T("Please enter the text for the tag : "),_T("this is a tag"), wxOK);

   if (dialog.ShowModal() == wxID_OK)
      AddTag(action_point.x, action_point.y, dialog.GetValue());
}

/////////////////////////////////////////////////////
void Network::OnAddLinkCon(wxCommandEvent& WXUNUSED(event))
{  
   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
   links[m_selLink].DrawLink( false, userScale );

   //int n = links[m_selLink].GetNumberOfPoints()+2;
   //linkline.resize(n);

   //unsigned long fromModuleID = links[m_selLink].GetFromModule();
   //unsigned int fromPort = links[m_selLink].GetFromPort();
   //*(linkline.GetPoint( 0 )) = GetPointForSelectedPlugin( fromModuleID, fromPort, "ouput" );

   VE_Conductor::GUI_Utilities::Polygon linkline;
   size_t i;
   for ( i=0; i< links[m_selLink].GetPoints()->size(); i++ )
      *(linkline.GetPoint( i )) = *(links[ m_selLink ].GetPoint( i ));
   
   //unsigned long toModuleID = links[m_selLink].GetToModule();
   //unsigned int toPort = links[m_selLink].GetToPort();
   //*(linkline.GetPoint( n-1 )) = GetPointForSelectedPlugin( toModuleID, toPort, "input" );
   VE_Conductor::GUI_Utilities::Polygon Near;
   linkline.nearpnt( action_point, Near );

   //size_t i;
   for ( i=0; i < linkline.GetNumberOfPoints()-1; i++)
      if (  ( linkline.GetPoint( i )->x <= Near.GetPoint( 0 )->x && 
            linkline.GetPoint( i+1 )->x >= Near.GetPoint( 0 )->x ) ||
            ( linkline.GetPoint( i )->x >= Near.GetPoint( 0 )->x && 
            linkline.GetPoint( i+1 )->x <= Near.GetPoint( 0 )->x )
         )
         break;

   size_t j;
   VE_Conductor::GUI_Utilities::Polygon temp;
   for ( j=1; j< linkline.GetNumberOfPoints()-1; ++j )
   {
      if ( j-1 == i )
	      temp.SetPoint( *(Near.GetPoint( 0 )) );
	   temp.SetPoint( *(linkline.GetPoint( j )) );
   }

   //Between the first link con and the port, and no cons yet
   if (j==1 && i==0) 
      temp.SetPoint( *(Near.GetPoint( 0 )) );

   //between the port and the las con
   if (j==linkline.GetNumberOfPoints()-1 && i==(j-1) && i!=0) 
      temp.SetPoint( *(Near.GetPoint( 0 )) );
  
  *(links[ m_selLink ].GetPolygon()) = temp;
  links[m_selLink].DrawLinkCon( true, userScale );
  m_selLink = -1;
  m_selLinkCon = -1;
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
    
}
/////////////////////////////////////////////////////
void Network::OnEditTag(wxCommandEvent& WXUNUSED(event))
{
   wxClientDC dc(this);
   PrepareDC(dc);

   dc.SetUserScale( userScale.first, userScale.second );

   wxString tag_text = *( tags[ m_selTag ].GetTagText() );
   wxTextEntryDialog dialog(this,_T("Tag Editor"), _T("Please enter the text for the tag : "),tag_text, wxOK);

   if (dialog.ShowModal() == wxID_OK)
   {
      tag_text=dialog.GetValue();
   }

   int w, h;
   dc.GetTextExtent( tag_text, &w, &h);

   *(tags[m_selTag].GetTagText()) = tag_text;
   tags[m_selTag].GetBoundingBox()->width = w;
   tags[m_selTag].GetBoundingBox()->height = h;
   tags[m_selTag].CalcTagPoly();
   //  Refresh(false);
   m_selTag = -1;
}

/////////////////////////////////////////////////////
void Network::OnDelTag(wxCommandEvent& WXUNUSED(event))
{
   int answer = wxMessageBox("Do you really want to delete this tag?", "Confirmation", wxYES_NO);
   if ( answer != wxYES )
   {
      return;
   }
   
   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
   
   std::vector< Tag >::iterator iter;
   int i;
   for ( iter = tags.begin(), i=0; iter != tags.end(); iter++, i++)
      if ( i == m_selTag )
      {
         tags.erase( iter );
         m_selTag=-1;
         break;
      }
   
   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
   //  Refresh(false);
   ReDrawAll();
}

/////////////////////////////////////////////////////
void Network::OnDelLink(wxCommandEvent& WXUNUSED(event))
{
   int answer=wxMessageBox("Do you really want to delete this link?", "Confirmation", wxYES_NO);
   if (answer!=wxYES)
      return;
   
   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);

   int i;
   std::vector< Link >::iterator iter;
   for (iter=links.begin(), i=0; iter!=links.end(); iter++, i++)
   {
      if (i==m_selLink)
      {
         links.erase(iter);
         m_selLink=-1;
         break;
      }
   }

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
   //  Refresh(false);
   ReDrawAll();
}

/////////////////////////////////////////////////////
void Network::OnDelLinkCon(wxCommandEvent& WXUNUSED(event))
{
   std::vector<wxPoint>::iterator iter;
   int answer, i;

   answer=wxMessageBox("Do you really want to delete this link connector?", "Confirmation", wxYES_NO);
   if (answer!=wxYES)
      return;

   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
   
   links[m_selLink].DrawLinkCon( false, userScale );

   for (iter=links[m_selLink].GetPoints()->begin(), i=0; iter!=links[m_selLink].GetPoints()->end(); iter++, i++)
      if ( i == m_selLinkCon )
      {
         links[m_selLink].GetPoints()->erase(iter);
         links[m_selLink].CalcLinkPoly();
         m_selLinkCon=-1;
         break;
      }

   links[m_selLink].DrawLinkCon( true, userScale );

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
   ReDrawAll();
}

/////////////////////////////////////////////////////
void Network::OnDelMod(wxCommandEvent& WXUNUSED(event))
{
   int answer=wxMessageBox("Do you really want to delete this module?", "Confirmation", wxYES_NO);
   if (answer!=wxYES)
      return;

   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);

   // Need to delete all links associated with this particular module
   // first, delete all the links connects to it
   std::vector< Link >::iterator iter3;
   for ( iter3=links.begin(); iter3!=links.end(); )
   {
      if ( 
            (iter3->GetFromModule() == m_selMod) || 
            (iter3->GetToModule() == m_selMod) 
         )
      {
	      links.erase( iter3 );
	   }
      else
         ++iter3;
   }
   
   //Now delete the plugin from the module and then remove from the map
   std::map< int, Module >::iterator iter;
   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      if (iter->first==m_selMod)
      {
	      delete modules[m_selMod].GetPlugin();
	      modules.erase(iter);
         if ( dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->IsConnectedToXplorer() )
         {
            VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair(  std::string("UNSIGNED INT") );
            dataValuePair->SetDataName( "Object ID" );
            dataValuePair->SetDataValue( static_cast< unsigned int >( m_selMod ) );
            VE_XML::Command* veCommand = new VE_XML::Command();
            veCommand->SetCommandName( std::string("DELETE_OBJECT_FROM_NETWORK") );
            veCommand->AddDataValuePair( dataValuePair );
            bool connected = dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->SendCommandStringToXplorer( veCommand );
            //Clean up memory
            delete veCommand;
         }         
	      m_selLink=-1;
	      break;
      }
   }

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);

   ReDrawAll();
}

/////////////////////////////////////
///// Selection Functions ///////////
/////////////////////////////////////

int Network::SelectMod( int x, int y )
{
   // This function checks to see which module your mouse is over based
   // on the x and y location of your mouse on the design canvas
   std::map< int, Module >::iterator iter;

   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      wxPoint temp;
      int i = iter->first;
      
      temp.x = x;
      temp.y = y;
      
      if ( modules[i].GetPolygon()->inside( temp ) )
	   {
         // I think...this means we have already been 
         // through here and is the same module selected -- mccdo
	      if ( m_selMod == i )
	      {
            // in the future send select module command -- mccdo
            return i;
         }
         // lets draw some ports sense the module is selected
	      DrawPorts( modules[i].GetPlugin(), true );
         // now we are officially selected
	      m_selMod = i;

	      return i;
	   }
   }

   return -1;
}

/////////////////////////////////////////////////////
void Network::UnSelectMod(wxDC &dc)
{
  DrawPorts( modules[m_selMod].GetPlugin(), false ); // wipe the ports
  
  ReDraw( dc );
  m_selMod = -1;
}

///////////////////////////////////////////////////////
int Network::SelectLink(int x, int y)
{
   wxPoint temp;
   temp.x = x; 
   temp.y = y;
   for ( unsigned int i=0; i<links.size(); i++)
   {
      if ( links[i].GetPolygon()->inside( temp ) )
	   {
         //draw link connectors
	      links[i].DrawLinkCon( true, userScale ); 
	      m_selLink = i;
	      return i;
	   }
   }
   return -1;
}

//////////////////////////////////////////////////////
void Network::UnSelectLink(wxDC &dc)
{
   //wipe link connectors
   links[m_selLink].DrawLinkCon( false, userScale );
   
   ReDraw(dc);
   m_selLink = -1;
   return;
}

//////////////////////////////////////////////////////
int Network::SelectTag(int x, int y)
{
   wxPoint temp;
   temp.x = x; 
   temp.y = y;
   for ( unsigned int i=0; i<tags.size(); i++)
   {
      if ( tags[i].GetPolygon()->inside( temp ) )
	   {
	      if (m_selTag == (int)i)
	         return i;
	      tags[i].DrawTagCon( true, userScale );
	      m_selTag = i;
	      return i;
	   }
   }
   return -1;
}

/////////////////////////////////////////////////
void Network::UnSelectTag(wxDC &dc)
{
  tags[m_selTag].DrawTagCon( false, userScale );
  
  ReDraw(dc);
  m_selTag = -1;
  return;
}

/////////////////////////////////////////////////
/////////////// Misc Functions //////////////////
/////////////////////////////////////////////////
void Network::SetXplorerInterface( VjObs_ptr veEngine )
{
   xplorerPtr = VjObs::_duplicate( veEngine );
}
/////////////////////////////////////////////////
void Network::CleanRect(wxRect box, wxDC &dc)
{
  wxBrush oldbrush = dc.GetBrush();
  wxPen oldpen = dc.GetPen();

  dc.SetBrush(*wxWHITE_BRUSH);
  dc.SetPen(*wxWHITE_PEN);
  
  dc.DrawRectangle(box.x, box.y, box.width, box.height);

  dc.SetBrush(oldbrush);
  dc.SetPen(oldpen);
}
/////////////////////////////////////////////////
wxPoint Network::GetFreePos(wxRect bbox)
{
   // Checks to see if there are any free spaces on the design canvas
   const int distx=10;
   const int disty=10;
   int limitx = 5;
   int limity = 5;
   int try_x=0;
   int try_y=0;
   wxPoint result(distx,disty);
   wxRect testbox=bbox;

   for ( int i=0; i<(int)sbboxes.size(); i++)
   {
      testbox.Offset(result.x-testbox.x, result.y-testbox.y);
     
      if (testbox.Intersects(sbboxes[i]))
	   {
	      if ((try_y < limity) && (try_y<(int)sbboxes.size()))
	      {
	         result.y=sbboxes[try_y].GetBottom()+disty;
	         try_y++;
	         i=-1;
	         continue;
	      }
	      else if ((try_x < limitx) && (try_x<(int)sbboxes.size()))
	      {
	         result.x=sbboxes[try_x].GetRight()+distx;
	         result.y=disty;
	         try_x++;
	         try_y=0;
	         i=-1;
	         continue;
	      }
	      else
	      {
	         try_y = limity;
	         limity+=5;
	         try_x = 0;
	         limitx+=limitx;
	         result.y=sbboxes[try_y].GetBottom()+disty;
	         try_y++;
	         i=-1;
	         continue;
	      }
	   }
   }
 
   return result;
}


////////////////////////////////////////////////////
////////// Move and Drop Functions /////////////////
////////////////////////////////////////////////////

void Network::MoveModule(int x, int y, int mod, wxDC &dc)
{
  REI_Plugin *cur_module;
  wxRect bbox;
  int xunit, yunit;
  int xpos, ypos, oldxpos, oldypos;
  int w, h, ex, ey, sx, sy;
  bool scroll = false; 

  GetScrollPixelsPerUnit(&xunit, &yunit);
  GetViewStart(&xpos, &ypos);
  GetVirtualSize(&sx, &sy);
  GetClientSize(&w, &h);

  xpos = (int)( 1.0 * xpos / userScale.first );
  ypos = (int)( 1.0 * ypos / userScale.first );
  oldxpos = xpos;
  oldypos = ypos;

  sx = (int)( 1.0*sx / userScale.first );
  sy = (int)( 1.0*sy / userScale.first );
  w = (int)( 1.0*w / userScale.first );
  h = (int)( 1.0*h / userScale.first ); 
  
  ex=xpos*xunit+w;
  ey=ypos*yunit+h;

  if (mod < 0) // no module is selected
    return; 
  
  cur_module = modules[mod].GetPlugin();
      
  bbox = cur_module->GetBBox(); //Get the Boundoing box of the modul 
  
  if (x>ex-bbox.width)
    xpos+=1;//userScale.first;
  else  if (x<(ex-w)+relative_pt.x)
    xpos-=1;//userScale.first;
  
  if (y>ey-bbox.height)
    ypos+=1;//userScale.first;
  if (y<(ey-h+relative_pt.y))
    ypos-=1;//userScale.first;
  
   if (x-relative_pt.x+bbox.width > sx)
   {
      GetNumUnit()->first+=2;
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
   }

   if (y-relative_pt.y+bbox.height > sy)
   {
      GetNumUnit()->second+=2;
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
   }

  cur_module->SetPos(wxPoint(x-relative_pt.x, y-relative_pt.y));
   //wipe off the old link connects with this module
   //Draw the links for a particular module
   for ( size_t i=0; i< links.size(); ++i )
   {
      if ( (links.at( i ).GetFromModule() == mod) )
      {
         links.at( i ).DrawLink( false, userScale );
         wxPoint pos = GetPointForSelectedPlugin( mod, links.at( i ).GetFromPort(), "output" );
         *(links.at( i ).GetPoint( 0 )) = pos;
      }
      //if the modules are the same
      if ( (links.at( i ).GetToModule() == mod) )
      {
         links.at( i ).DrawLink( false, userScale );
         wxPoint pos = GetPointForSelectedPlugin( mod, links.at( i ).GetToPort(), "input" );
         *(links.at( i ).GetPoint( links.at( i ).GetPoints()->size()-1 )) = pos;
      }
   }
    
  if ((bbox.x-3.0/userScale.first)>0)
    bbox.x-=(int)( 3.0/userScale.first );
  else
    bbox.x=0;
  
  if ((bbox.y-3.0/userScale.first)>0)
    bbox.y-=(int)( 3.0/userScale.first );
  else
    bbox.y=0;
  
  bbox.width+=(int)( 3.0/userScale.first );
  bbox.height+=(int)( 3.0/userScale.first );

  CleanRect(bbox, dc);  
  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = (int)( 1.0 * xpos * userScale.first );
      ypos = (int)( 1.0 * ypos * userScale.first );
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
 
}

/////////////////////////////////////////////////////////////
void Network::DropModule(int ix, int iy, int mod )
{
  wxRect bbox; //Bounding box  
  int sx, sy;
  double r;
  int vx, vy;
  int x, y;
  REI_Plugin * cur_module;
  bool scroll = false; 

  //In drag mode
  if (mod < 0) // no module is selected
    return; 
  
  GetVirtualSize(&sx, &sy);
  sx = (int)( 1.0*sx / userScale.first );
  sy = (int)( 1.0*sy / userScale.first );
  
  if (ix<relative_pt.x)
    x=relative_pt.x;
  else
    x=ix;
  if (iy<relative_pt.y)
    y=relative_pt.y;
  else
    y=iy;

  cur_module = modules[mod].GetPlugin();
      
  bbox = cur_module->GetBBox(); //Get the Boundoing box of the modul 

  //  bbox.x = dc.LogicalToDeviceX(bbox.x);
  //  bbox.y = dc.LogicalToDeviceY(bbox.y);
  bbox.x = 0;
  bbox.y = 0;
  GetViewStart(&vx,&vy);
  //  vx= vx / userScale.first;
  //  vy = vy / userScale.first;
  
  if (x-relative_pt.x+bbox.width > sx)
    {
      r=(1.0*(x-relative_pt.x+bbox.width) / sx);
      GetNumUnit()->first=int (r*GetNumUnit()->first+1);
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
      vx = GetNumUnit()->first;
    }
  if (y-relative_pt.y+bbox.height > sy)
    {
      r=(1.0*(y-relative_pt.y+bbox.width) / sy);
      GetNumUnit()->second=int (r*GetNumUnit()->second+1);
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
      vy = GetNumUnit()->second;
    }
  
  cur_module->SetPos(wxPoint(x-relative_pt.x, y-relative_pt.y));

   //num = cur_module->GetNumPoly();
   //tmppoly.resize(num);
   VE_Conductor::GUI_Utilities::Polygon tmppoly;
   POLY oldPoly;
   oldPoly.resize( cur_module->GetNumPoly() );
   cur_module->GetPoly( oldPoly );
   *(tmppoly.GetPolygon()) = oldPoly;
   tmppoly.TransPoly( x-relative_pt.x, y-relative_pt.y, *(modules[mod].GetPolygon()) );

   //Recalc links poly as well for a particular module
   for ( size_t i=0; i< links.size(); ++i )
   {
      if ( (links.at( i ).GetFromModule() == mod) || (links.at( i ).GetToModule() == mod) )
         links.at( i ).CalcLinkPoly();
   }

  //if ((bbox.x-3)>0)
  //  bbox.x-=3;
  //  else
  //  bbox.x=0;
  
  //if ((bbox.y-3)>0)
  //   bbox.y-=3;
  //else
  //  bbox.y=0;
  
  //  bbox.x =0;
  //  bbox.y =0;
  //  bbox.width=sx;
  //  bbox.height=sy;

  //xpos = 1.0 * xpos * userScale.first;
  //ypos = 1.0 * ypos * userScale.first;
      
  //  Scroll(vx, vy);  
  //CleanRect(bbox, dc);
  ReDrawAll();

}

/////////////////////////////////////////////////////////////////////////
void Network::TryLink(int x, int y, int mod, int pt, wxDC& dc, bool flag)
{
   //int xoff, yoff;
   wxPoint temp;
   POLY ports;
   wxRect bbox;
   static int dest_mod=-1;
   int i, t;
   std::map< int, Module >::iterator iter;

   t=-1;

   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      i = iter->first;
      temp.x = x;
      temp.y = y;

      if ( modules[i].GetPolygon()->inside( temp ) && dest_mod!=mod)
      {
         t = i;
         break;
      }
   }

   iter=modules.find(dest_mod);	

   if (t!=dest_mod && iter!=modules.end())
      DrawPorts(modules[dest_mod].GetPlugin(), false); //wipe the ports

   dest_mod = t;

   DrawPorts(modules[mod].GetPlugin(), false); //wipe the ports

   wxPoint offSet;
   if ( flag )
   {
      DrawPorti(modules[mod].GetPlugin(), pt, flag);
      offSet = GetPointForSelectedPlugin( mod, pt, "input" );
   }
   else
   {
      DrawPorti(modules[mod].GetPlugin(), pt, flag);
      offSet = GetPointForSelectedPlugin( mod, pt, "output" );
   }

   dc.SetPen(*wxWHITE_PEN);
   dc.DrawLine( offSet.x, offSet.y, xold, yold);
   ReDraw(dc);

   if ( dest_mod >=0 )
      DrawPorts( modules[dest_mod].GetPlugin(), true); //draw the ports

   dc.SetPen(*wxBLACK_PEN);
   dc.DrawLine( offSet.x, offSet.y, x, y);

   xold = x;
   yold = y;
}

////////////////////////////////////////////////////////////////////////
void Network::DropLink(int x, int y, int mod, int pt, wxDC &dc, bool flag)
{
   //first check if there is an apropriate port on the destination position
   //in the mean time, also find out the wipe off line's start position 
   //int xoff, yoff;

   PORT ports;
   wxRect bbox;
   wxPoint temp;
   int dest_mod, dest_port;
   int i;
   std::map< int, Module >::iterator iter;

   dest_mod = dest_port = -1;

   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      i = iter->first;
      temp.x = x;
      temp.y = y;
      if ( modules[i].GetPolygon()->inside( temp ) )
      {
         dest_mod = i;
         break;
      }
   }

   if (dest_mod>=0)
   {
      DrawPorts( modules[dest_mod].GetPlugin(), false ); //Wipe off the port rect
      bbox = modules[dest_mod].GetPlugin()->GetBBox();

      temp.x = x - bbox.x;
      temp.y = y - bbox.y;
   }

   // If input port
   wxPoint offSet;
   if ( flag )
   {
      DrawPorts(modules[mod].GetPlugin(), false); //Wipe off the port rect
      offSet = GetPointForSelectedPlugin( mod, pt, "input" );

      if (dest_mod>=0)
      {
         ports.resize( modules[dest_mod].GetPlugin()->GetNumOports() );
         modules[dest_mod].GetPlugin()->GetOPorts(ports);
   
         for (i=0; i<(int)ports.size(); i++)
         {
            wxPoint tempPoint( ports[i].GetPortLocation()->GetPoint().first, ports[i].GetPortLocation()->GetPoint().second );
            if (computenorm(temp, tempPoint)<=10) 
            {
               //Also, we need to check if port Type is the same
				if (IsPortCompatible(dest_mod, i, mod, pt))
				//if (IsPortCompatible(mod, pt, dest_mod, i))
				{
					dest_port = i;
					break;
				}
            }
         }
      }
   }
   else    // If ouput port
   {
      DrawPorts(modules[mod].GetPlugin(), false); //Wipe off the port rect
      offSet = GetPointForSelectedPlugin( mod, pt, "output" );

      // check if the drop point is a out port
      if (dest_mod>=0)
      {
         ports.resize( modules[dest_mod].GetPlugin()->GetNumIports() );
         modules[dest_mod].GetPlugin()->GetIPorts(ports);
         for (i=0; i<(int)ports.size(); i++)
         {
            wxPoint tempPoint( ports[i].GetPortLocation()->GetPoint().first, ports[i].GetPortLocation()->GetPoint().second );
            if (computenorm(temp, tempPoint)<=10)
            {
				//if (IsPortCompatible(dest_mod, i, mod, pt))
				if (IsPortCompatible(mod, pt, dest_mod, i))
				{
					dest_port = i;
					break;
				}
            }
         }
      }
   }  

   //Wipe off the test line
   dc.SetPen(*wxWHITE_PEN);
   dc.DrawLine( offSet.x, offSet.y, xold, yold);

   // if it is a good link
   // and a user can not link to itself
   if (dest_mod>=0 && dest_port>=0 && ( (dest_mod!=mod) || (dest_port!=pt) ) )
   {
      Link ln( this );
      if ( flag ) // if input port
      {
         ln.SetToModule( mod );
         ln.SetToPort( pt );
         ln.SetFromModule( dest_mod );
         ln.SetFromPort( dest_port );
      }
      else // if output port
      {
         ln.SetToModule( dest_mod );
         ln.SetToPort( dest_port );
         ln.SetFromModule( mod );
         ln.SetFromPort( pt );
      }
   
      // check for duplicate links
      bool found = false;
      for (i=0; i<(int)links.size(); i++)
      {
         if ( links.at( i ) == ln )
         {
            found = true;
         }
      }

      if ( !found ) // no duplicate links are allowed
      {
         wxPoint pos;
         // Get first port point for the link
         pos = GetPointForSelectedPlugin( ln.GetFromModule(), ln.GetFromPort(), "output" );
         ln.SetPoint( &pos );//->push_back( GetPointForSelectedPlugin( ln.GetFromModule(), ln.GetFromPort(), "output" ) );

         // Get last port point for the link
         pos = GetPointForSelectedPlugin( ln.GetToModule(), ln.GetToPort(), "input" );
         ln.SetPoint( &pos );//->push_back( GetPointForSelectedPlugin( ln.GetToModule(), ln.GetToPort(), "input" ) );
         ln.CalcLinkPoly();
         links.push_back( ln );
      }
   }

   m_selMod = -1;
   m_selFrPort = -1;
   m_selToPort = -1;
   ReDrawAll();
}
/////////////////////////////////////////////////////////////////////
bool Network::IsPortCompatible(int frmod, int frport, int tomod, int toport)
{
	int num = 0;
	PORT ports;
    wxPoint tempPoint;

    num = modules[ frmod ].GetPlugin()->GetNumOports();	
	ports.resize(num);
    modules[ frmod ].GetPlugin()->GetOPorts( ports );
	std::string type1="";
	if (frport>=0 && frport<num)
		type1= ports[frport].GetPortType();
   
	num = modules[ tomod ].GetPlugin()->GetNumIports();	
	ports.resize(num);
    modules[ tomod ].GetPlugin()->GetIPorts( ports );
	std::string type2="";
	if (toport>=0 && toport<num)
		type2= ports[toport].GetPortType();
   
	if (type1==type2)
		return true;
	else
		return false;
}
/////////////////////////////////////////////////////////////////////
void Network::MoveLinkCon(int x, int y, int ln, int ln_con, wxDC& dc)
{
  int xunit, yunit;
  int xpos, ypos, oldxpos, oldypos;
  int w, h, ex, ey, sx, sy;
  bool scroll = false; 

  GetScrollPixelsPerUnit(&xunit, &yunit);
  GetViewStart(&xpos, &ypos);
  GetVirtualSize(&sx, &sy);
  GetClientSize(&w, &h);
  
  oldxpos = xpos;
  oldypos = ypos;

  sx = (int)( 1.0*sx / userScale.first );
  sy = (int)( 1.0*sy / userScale.first );
  w = (int)( 1.0*w / userScale.first );
  h = (int)( 1.0*h / userScale.first ); 

  ex=xpos*xunit+w;
  ey=ypos*yunit+h;
      
  if (x>ex)
    xpos+=1;
  if (x<(ex-w))
    xpos-=1;
  
  if (y>ey)
    ypos+=1;
  if (y<(ey-h))
    ypos-=1;
    
  if (x > sx)
    {
      GetNumUnit()->first+=2;
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
    }
  if (y > sy)
    {
      GetNumUnit()->second+=2;
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
    }

  //erase the original link;
  links[ln].DrawLink( false, userScale );
  links[ln].DrawLinkCon( false, userScale );
  *(links[ln].GetPoint( ln_con )) = wxPoint(x,y);
 
   if ( oldxpos!=xpos || oldypos!=ypos || scroll)
   {
      xpos = (int)( 1.0 * xpos * userScale.first );
      ypos = (int)( 1.0 * ypos * userScale.first );

      Scroll(xpos, ypos);
      ReDrawAll();
   }
   else
      ReDraw(dc);

   links[ln].DrawLinkCon( true, userScale );
}

//////////////////////////////////////////////////////////////////////
void Network::DropLinkCon(int x, int y, int ln, int ln_con, wxDC &dc)
{
  int sx, sy;
  double r;
  int vx, vy;
  bool scroll = false; 
  
  GetVirtualSize(&sx, &sy);

  sx = (int)( 1.0*sx / userScale.first );
  sy = (int)( 1.0*sy / userScale.first );
  //  w = w / userScale.first;
  //h = h / userScale.first; 
      
   GetViewStart(&vx,&vy);
   if (x > sx)
   {
      r=(1.0*x/sx);
      GetNumUnit()->first=int (r*GetNumUnit()->first+1);
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
      vx = GetNumUnit()->first;
   }

   if (y > sy)
   {
      r=(1.0*y/sy);
      GetNumUnit()->second=int (r*GetNumUnit()->second+1);
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
      vy = GetNumUnit()->second;
   }
  
  *(links[ln].GetPoint( ln_con )) = wxPoint(x,y);

  links[ln].CalcLinkPoly();

  //  Scroll(vx, vy);
  ReDraw(dc);
}

//////////////////////////////////////////////////////////////////
void Network::MoveTagCon(int x, int y, int t, int t_con, wxDC& dc)
{
  int xunit, yunit;
  int xpos, ypos;
  int oldxpos, oldypos;
  int w, h, ex, ey, sx, sy;
  bool scroll = false; 

  GetScrollPixelsPerUnit(&xunit, &yunit);
  GetViewStart(&xpos, &ypos);
  oldxpos = xpos;
  oldypos = ypos;
  GetVirtualSize(&sx, &sy);
  GetClientSize(&w, &h);
  
  sx = (int)( 1.0*sx / userScale.first );
  sy = (int)( 1.0*sy / userScale.first );
  w = (int)( 1.0*w / userScale.first );
  h = (int)( 1.0*h / userScale.first ); 

  ex=xpos*xunit+w;
  ey=ypos*yunit+h;
      
  if (x>ex)
    xpos+=1;
  if (x<(ex-w))
    xpos-=1;
  
  if (y>ey)
    ypos+=1;
  if (y<(ey-h))
    ypos-=1;
    
  if (x > sx)
    {
      GetNumUnit()->first+=2;
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
    }
  if (y > sy)
    {
      GetNumUnit()->second+=2;
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
    }

  //erase the original Tag;
  tags[t].DrawTag( false, userScale );
  tags[t].DrawTagCon( false, userScale );
  *(tags[t].GetConnectorsPoint( t_con ))=wxPoint(x,y);
  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = (int)( 1.0 * xpos * userScale.first );
      ypos = (int)( 1.0 * ypos * userScale.first );
      
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
  tags[t].DrawTagCon( true, userScale );
  
}

//////////////////////////////////////////////////////////////////
void Network::DropTagCon(int x, int y, int t, int t_con, wxDC &dc)
{
  int sx, sy;
  double r;
  int vx, vy;
  
  bool scroll = false; 

  GetVirtualSize(&sx, &sy);
 
  sx = (int)( 1.0*sx / userScale.first );
  sy = (int)( 1.0*sy / userScale.first );
  //  w = w / userScale.first;
  //  h = h / userScale.first; 
     
  GetViewStart(&vx,&vy);
  if (x > sx)
    {
      r=(1.0*x/sx);
      GetNumUnit()->first=int (r*GetNumUnit()->first+1);
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      vx = GetNumUnit()->first;
      scroll = true;
    }
  if (y > sy)
    {
      r=(1.0*y/sy);
      GetNumUnit()->second=int (r*GetNumUnit()->second+1);
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
      vy = GetNumUnit()->second;
    }
  
  *(tags[t].GetConnectorsPoint( t_con )) = wxPoint(x,y);

  tags[t].CalcTagPoly();

  //  Scroll(vx, vy);
  ReDraw(dc);
}

///////////////////////////////////////////////////////
void Network::MoveTag(int x, int y, int t, wxDC &dc)
{
  int xunit, yunit;
  int xpos, ypos, oldxpos, oldypos;
  int w, h, ex, ey, sx, sy;
  bool scroll=false;
  GetScrollPixelsPerUnit(&xunit, &yunit);
  GetViewStart(&xpos, &ypos);
  GetVirtualSize(&sx, &sy);
  GetClientSize(&w, &h);
  
  oldxpos = xpos;
  oldypos = ypos;

  sx = (int)( 1.0*sx / userScale.first );
  sy = (int)( 1.0*sy / userScale.first );
  w = (int)( 1.0*w / userScale.first );
  h = (int)( 1.0*h / userScale.first ); 

  ex=xpos*xunit+w;
  ey=ypos*yunit+h;

  if (x>ex)
    xpos+=1;
  if (x<(ex-w))
    xpos-=1;
  
  if (y>ey)
    ypos+=1;
  if (y<(ey-h))
    ypos-=1;
    
  if (x > sx)
    {
      GetNumUnit()->first+=2;
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
    }
  if (y > sy)
    {
      GetNumUnit()->second+=2;
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
    }

  //erase the original Tag;
  tags[t].DrawTag( false, userScale );
  tags[t].DrawTagCon( false, userScale );

  tags[t].GetBoundingBox()->x = x-tag_rpt.x;
  tags[t].GetBoundingBox()->y = y-tag_rpt.y;

  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = (int)( 1.0 * xpos * userScale.first );
      ypos = (int)( 1.0 * ypos * userScale.first );
      
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
  tags[t].DrawTagCon( true, userScale );
  
}

/////////////////////////////////////////////////////
void Network::DropTag(int x, int y, int t, wxDC &dc)
{
  int sx, sy;
  double r;
  int vx, vy;
  bool scroll = false; 
  
  GetVirtualSize(&sx, &sy);

  sx = (int)(1.0*sx / userScale.first);
  sy = (int)(1.0*sy / userScale.first);
  //  w = w / userScale.first;
  //  h = h / userScale.first; 
      
  GetViewStart(&vx,&vy);
  if (x > sx)
    {
      r=(1.0*x/sx);
      GetNumUnit()->first=int (r*GetNumUnit()->first+1);
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
      vx = GetNumUnit()->first;
    }
  if (y > sy)
    {
      r=(1.0*y/sy);
      GetNumUnit()->second=int (r*GetNumUnit()->second+1);
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
      vy = GetNumUnit()->second;
    }
  
  tags[t].GetBoundingBox()->x = x - tag_rpt.x;
  tags[t].GetBoundingBox()->y = y - tag_rpt.y;

  tags[t].CalcTagPoly();

  //  Scroll(vx, vy);
  ReDraw(dc);
}

//////////////////////////////////////////////////////
///////// Add to Network Funtions ////////////////////
//////////////////////////////////////////////////////

void Network::AddTag(int x, int y, wxString text)
{
   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
   Tag t( this );
   int w, h;
   wxClientDC dc(this);
   PrepareDC(dc);
   dc.SetUserScale( userScale.first, userScale.second );

   t.GetConnectorsPoint( 0 )->x = x; 
   t.GetConnectorsPoint( 0 )->y = y;

   t.GetConnectorsPoint( 1 )->x = x+60;
   if ( y > 40 )
      t.GetConnectorsPoint( 1 )->y = y-20;
   else
      t.GetConnectorsPoint( 1 )->y = y+20;

   dc.GetTextExtent(text, &w, &h);

   *(t.GetTagText()) = text;
   t.GetBoundingBox()->x=x+80;
   t.GetBoundingBox()->y = t.GetConnectorsPoint( 1 )->y - h/2;
   t.GetBoundingBox()->width = w;
   t.GetBoundingBox()->height = h;
   t.CalcTagPoly();
   tags.push_back(t);

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
   ReDraw(dc);
}

//////////////////////////////////////////////////////////////
void Network::AddtoNetwork(REI_Plugin *cur_module, std::string cls_name)
{
  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
  Module mod;
  POLY tmpPoly;
  int num;

  wxRect bbox; //Bounding box  

  bbox = cur_module->GetBBox(); //Get the Boundoing box of the modul 

  cur_module->SetPos( GetFreePos(bbox) ); //Set the new modules position to be a free space allocated by the network according to its bounding box
  bbox = cur_module->GetBBox();
  mod.SetPlugin( cur_module );

   num = cur_module->GetNumPoly();
   tmpPoly.resize(num);
   cur_module->GetPoly(tmpPoly); 
   VE_Conductor::GUI_Utilities::Polygon newPolygon;
   *(newPolygon.GetPolygon()) = tmpPoly;

   newPolygon.TransPoly( bbox.x, bbox.y, *(mod.GetPolygon()) ); //Make the network recognize its polygon 
   mod.SetClassName( cls_name );

   int id;
   std::map<int, Module>::iterator mit;
   while (1)
   {
      id = wxNewId();
      if ( id > 9999 )
         id=id%9999;
   
      mit = modules.find(id);
      if ( mit == modules.end() )
         break;
   };


  modules[id]=mod;

  modules[id].GetPlugin()->SetID(id);  
  //modules.push_back(mod);
  sbboxes.push_back(bbox);
  //  for (i=0; i<modules.size(); i++)
  
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( userScale.first, userScale.second );
  ReDraw(dc);
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
}

////////////////////////////////////////
/////// Draw Functions /////////////////
////////////////////////////////////////


void Network::ReDrawAll()
{
  wxClientDC dc(this);
  PrepareDC(dc);

  //  box.x = box.y=0;
  
  //  box.width = dc.MaxX();
  //  box.height = dc.MaxY();
  //  dc.SetPen(*wxWHITE_PEN);
  //  dc.SetBrush(*wxWHITE_BRUSH);
  //  CleanRect(box, dc); 
  dc.SetUserScale(userScale.first, userScale.second);
  dc.SetBackground(*wxWHITE_BRUSH);
  dc.Clear();
  dc.SetPen(*wxBLACK_PEN);
  dc.SetBrush(*wxWHITE_BRUSH);
  
  ReDraw(dc);
}

/////////////////////////////////
void Network::ReDraw(wxDC &dc)
{
   // this function Redraw the design canvas
   dc.SetPen(*wxBLACK_PEN);
   dc.SetBrush(*wxWHITE_BRUSH);
   dc.SetBackground(*wxWHITE_BRUSH);
   //dc.Clear();
   //dc.SetBackgroundMode(wxSOLID);

   // redraw all the active plugins
   std::map<int, Module>::iterator iter;
   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      modules[ iter->first ].GetPlugin()->DrawIcon(&dc);
      modules[ iter->first ].GetPlugin()->DrawID(&dc);
   }

   // draw all the links
   for ( size_t i = 0; i < links.size(); ++i )
      links[i].DrawLink( true, userScale );

   // draw all the links
   for ( size_t i = 0; i < tags.size(); ++i )
      tags[i].DrawTag( true, userScale );
}

/////////////////////////////////////////////////////////////////////
void Network::DrawPorts( REI_Plugin* cur_module, bool flag )
{
   // flag sets whether we we are erasing the ports or not 
   // This function draws the input and output ports on a selected module
   // that is on the design canvas
   if (!cur_module)
      return;

   size_t i;
   wxPoint bport[4];
   wxCoord xoff, yoff;
   int num;

   wxClientDC dc(this);
   PrepareDC(dc);
   dc.SetUserScale( userScale.first, userScale.second );

   bport[0]=wxPoint(0,0);
   bport[1]=wxPoint(10,0);
   bport[2]=wxPoint(10,10);
   bport[3]=wxPoint(0,10);


   wxRect bbox = cur_module->GetBBox();

   wxBrush old_brush = dc.GetBrush();
   wxPen old_pen = dc.GetPen();

   if (flag)
   {
      dc.SetBrush(*wxRED_BRUSH);
      dc.SetPen(*wxBLACK_PEN);
	  dc.SetTextForeground(*wxBLACK);
   }
   else
   {
      dc.SetBrush(*wxWHITE_BRUSH);
      dc.SetPen(*wxWHITE_PEN);
	  dc.SetTextForeground(*wxWHITE);
   }

   PORT ports;
   num = cur_module->GetNumIports();
   ports.resize(num);
   cur_module->GetIPorts(ports);
   
   wxString text;
   int w, h;
   
   for (i=0; i<(int)ports.size(); i++)
   {
       wxPoint tempPoint( ports[i].GetPortLocation()->GetPoint().first, ports[i].GetPortLocation()->GetPoint().second );
      // I believe this means move the points in from the edge of the icon
      // by 3 pixles
      // bbox.x returns the global x location and the ports.x returns the x location with respect to bbox.x
      // the same is also true for the y values 
      xoff = tempPoint.x+bbox.x-3;
      yoff = tempPoint.y+bbox.y-3;

      // draw the polygon 
      dc.DrawPolygon(4, bport, xoff, yoff);  

	  //also, need to draw port type
	  text = ports[i].GetPortType().c_str();
	  dc.GetTextExtent( text, &w, &h);
	  dc.DrawText( text, xoff-w-2, yoff);
   }
   
   if ( flag )
   {
      dc.SetBrush(*wxCYAN_BRUSH);
   }
   else
   {
      ; //keep the white brush
   }

   // do the same thing as we did for the input ports
   num = cur_module->GetNumOports();
   ports.resize(num);
   cur_module->GetOPorts(ports);

   for ( i=0; i < ports.size(); i++)
   { 
       wxPoint tempPoint( ports[i].GetPortLocation()->GetPoint().first, ports[i].GetPortLocation()->GetPoint().second );
      xoff = tempPoint.x+bbox.x-3;
      yoff = tempPoint.y+bbox.y-3;

      dc.DrawPolygon(4, bport, xoff, yoff);      
	  //also, need to draw port type
	  text = ports[i].GetPortType().c_str();
	  dc.GetTextExtent( text, &w, &h);
	  dc.DrawText( text, xoff+12, yoff );
   }

   /* if ((bbox.x-3)>0)
   bbox.x-=3;
   else
   bbox.x=0;

   if ((bbox.y-3)>0)
   bbox.y-=3;
   else
   bbox.y=0;

   bbox.width+=3;
   bbox.height+=3;
   */
   // restore the default brush and pen settings as stored initially
   dc.SetBrush(old_brush);
   dc.SetPen(old_pen);
}

///////////////////////////////////////////////////////////////////////
void Network::DrawPorti(REI_Plugin * cur_module, int index, bool flag)
{
   // used by trylink only which redraws things only if we are draggin a module
   // draw either the input or output ports for an specific port index in the module
   PORT ports;
   int num;

   if ( !cur_module )
      return;
   wxClientDC dc(this);
   wxPoint bport[4];
   wxCoord xoff, yoff;
   wxRect bbox;

   PrepareDC(dc);
   dc.SetUserScale( userScale.first, userScale.second );

   bport[0]=wxPoint(0,0);
   bport[1]=wxPoint(10,0);
   bport[2]=wxPoint(10,10);
   bport[3]=wxPoint(0,10);

   bbox = cur_module->GetBBox();
   wxBrush old_brush=dc.GetBrush();

   dc.SetBrush(*wxRED_BRUSH);
   
   if (flag)
   {
      num = cur_module->GetNumIports();
      ports.resize(num);
      cur_module->GetIPorts(ports);
      dc.SetBrush(*wxRED_BRUSH);
   }
   else
   {
      num = cur_module->GetNumOports();
      ports.resize(num);
      cur_module->GetOPorts(ports);
      dc.SetBrush(*wxCYAN_BRUSH);
   }
  
   wxPoint tempPoint( ports[index].GetPortLocation()->GetPoint().first, ports[index].GetPortLocation()->GetPoint().second );
   xoff = tempPoint.x+bbox.x-3;
   yoff = tempPoint.y+bbox.y-3;
      
   dc.DrawPolygon(4, bport, xoff, yoff);      
  
   dc.SetBrush( old_brush );
}

/////////////////////////////////////////////////////////
////// Math Functions for the points and polygons ///////
/////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////
double Network::computenorm( wxPoint pt1, wxPoint pt2 )
{
  return sqrt(double((pt1.x - pt2.x)*(pt1.x - pt2.x) + (pt1.y - pt2.y)*(pt1.y - pt2.y)));
}

//////////////////////////////////////////////
//////// Save and Load Functions /////////////
//////////////////////////////////////////////
std::string Network::Save( std::string fileName )
{
   // Here we wshould loop over all of the following
   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   //  Newtork
   if ( veNetwork )
      delete veNetwork;
   
   veNetwork = new VE_Model::Network();
   nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( veNetwork, "veNetwork" ) );

   veNetwork->GetDataValuePair( -1 )->SetData( "m_xUserScale", userScale.first );
   veNetwork->GetDataValuePair( -1 )->SetData( "m_yUserScale", userScale.second );
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixX", static_cast< long int >( numPix.first ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixY", static_cast< long int >( numPix.second ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitX", static_cast< long int >( numUnit.first ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitY", static_cast< long int >( numUnit.second ) );

   for ( size_t i = 0; i < links.size(); ++i )
   {
      VE_Model::Link* xmlLink = veNetwork->GetLink( -1 );
      //xmlLink->GetFromPort()->SetData( modules[ links[i].GetFromModule() ].GetPlugin()->GetModelName(), links[i].GetFromPort() );
      //xmlLink->GetToPort()->SetData( modules[ links[i].GetToModule() ].pl_mod->GetModelName(), links[i].GetToPort() );
      xmlLink->GetFromModule()->SetData( modules[ links[i].GetFromModule() ].GetClassName(), static_cast< long int >( links[i].GetFromModule() ) );
      xmlLink->GetToModule()->SetData( modules[ links[i].GetToModule() ].GetClassName(), static_cast< long int >( links[i].GetToModule() ) );
      *(xmlLink->GetFromPort()) = static_cast< long int >( links[i].GetFromPort() );
      *(xmlLink->GetToPort()) = static_cast< long int >( links[i].GetToPort() );

      //Try to store link cons,
      //link cons are (x,y) wxpoint
      //here I store x in one vector and y in the other
      for ( size_t j = 0; j < links[ i ].GetNumberOfPoints(); ++j )
	   {
         xmlLink->GetLinkPoint( j )->SetPoint( std::pair< unsigned int, unsigned int >( links[ i ].GetPoint( j )->x, links[ i ].GetPoint( j )->y ) );
      }
   }

   //  Models
   std::map< int, Module >::iterator iter;
   for ( iter=modules.begin(); iter!=modules.end(); ++iter )
   {
      modules[ iter->first ].GetPlugin()->SetID( iter->first );
      nodes.push_back( 
                  std::pair< VE_XML::XMLObject*, std::string >( 
                  modules[ iter->first ].GetPlugin()->GetVEModel(), "veModel" ) 
                     );
      dynamic_cast< VE_Model::Model* >( nodes.back().first )->SetModelName( modules[ iter->first ].GetClassName() );
   }

   //  tags
   /*for ( size_t i = 0; i < veTagVector.size(); ++i )
   {
      delete veTagVector.at( i );
   }
   veTagVector.clear();

   for ( size_t i = 0; i < tags.size(); ++i )
   {
      std::pair< unsigned int, unsigned int > pointCoords;

      veTagVector.push_back( new VE_Model::Tag( doc ) );

      veTagVector.back()->SetTagText( tags.back().text.c_str() );

      pointCoords.first = tags.back().cons[0].x;
      pointCoords.second = tags.back().cons[0].y;
      veTagVector.back()->GetTagPoint( 0 )->SetPoint( pointCoords );

      pointCoords.first = tags.back().cons[1].x;
      pointCoords.second = tags.back().cons[1].y;
      veTagVector.back()->GetTagPoint( 1 )->SetPoint( pointCoords );

      pointCoords.first = tags.back().box.x;
      pointCoords.second = tags.back().box.y;
      veTagVector.back()->GetTagPoint( 2 )->SetPoint( pointCoords );
   }

   for ( size_t i = 0; i < tags.size(); ++i )
   {
      doc->getDocumentElement()->appendChild
         ( 
            veTagVector.at( i )->GetXMLData( "veTag" )
         );
   }*/

   VE_XML::XMLReaderWriter netowrkWriter;
   netowrkWriter.UseStandaloneDOMDocumentManager();
   netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );

   return fileName;
}
////////////////////////////////////////////////////////
void Network::New()
{
   // Just clear the design canvas
   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);

   links.clear();

   std::map<int, Module>::iterator iter;
   for ( iter=modules.begin(); iter!=modules.end(); ++iter )
   {
      delete modules[ iter->first ].GetPlugin();
      //Delete it from xplorer as well
      if ( dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->IsConnectedToXplorer() )
      {
         VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair(  std::string("UNSIGNED INT") );
         dataValuePair->SetDataName( "Object ID" );
         dataValuePair->SetDataValue( static_cast< unsigned int >( iter->first ) );
         VE_XML::Command* veCommand = new VE_XML::Command();
         veCommand->SetCommandName( std::string("DELETE_OBJECT_FROM_NETWORK") );
         veCommand->AddDataValuePair( dataValuePair );
         bool connected = dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->SendCommandStringToXplorer( veCommand );
         //Clean up memory
         delete veCommand;
      }
   }
   modules.clear();

   tags.clear();

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);

   Refresh();
}
////////////////////////////////////////////////////////
void Network::Load( std::string xmlNetwork )
{
   //Get a new canvas first to cleanup memory
   this->New();
   //Now...lets process some files
   _fileProgress = new wxProgressDialog(wxString("Translation Progress"),
                  "Load...", 
                  100,this,
                  wxPD_AUTO_HIDE|wxPD_SMOOTH|wxPD_ELAPSED_TIME|wxPD_ESTIMATED_TIME);
   ::wxBeginBusyCursor();
   //::wxSafeYield();
   //LoadThread* loadThread = new LoadThread( this );
   //loadThread->SetFileName( xmlNetwork );
   tempXMLNetworkData = xmlNetwork;
   //wxThreadHelper::Create();
   //this->GetThread()->Run();
   CreateNetwork( xmlNetwork );
   ::wxEndBusyCursor();
}
////////////////////////////////////////////////////////
void Network::CreateNetwork( std::string xmlNetwork )
{
   // Just clear the design canvas
   //while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR) { ; }
   // Start the busy cursor
   // Load from the nt file loaded through wx
   // Get a list of all the command elements   
   _fileProgress->Update( 10, "start loading" );
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();

   if ( xmlNetwork.size() < 512 )
   {
      networkWriter.ReadFromFile();
   }
   else
   {
      networkWriter.ReadFromString();
   }

   networkWriter.ReadXMLData( xmlNetwork, "Model", "veNetwork" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

   // do this for network
   if ( veNetwork )
      delete veNetwork;
   
   // we are expecting that a network will be found
   if ( !objectVector.empty() )
   {
      veNetwork = dynamic_cast< VE_Model::Network* >( objectVector.at( 0 ) );
   }
   else
   {
      wxMessageBox( "Improperly formated ves file.", 
                        "VES File Read Error", wxOK | wxICON_INFORMATION );
   }

   long int tempScaleInfo;
   veNetwork->GetDataValuePair( 0 )->GetData( (userScale.first)  );
   veNetwork->GetDataValuePair( 1 )->GetData( (userScale.second) );
   veNetwork->GetDataValuePair( 2 )->GetData( tempScaleInfo );
   numPix.first = tempScaleInfo;
   veNetwork->GetDataValuePair( 3 )->GetData( tempScaleInfo );
   numPix.second = tempScaleInfo;
   veNetwork->GetDataValuePair( 4 )->GetData( tempScaleInfo );
   numUnit.first = tempScaleInfo;
   veNetwork->GetDataValuePair( 5 )->GetData( tempScaleInfo );
   numUnit.second = tempScaleInfo;

   links.clear();

   for ( size_t i = 0; i < veNetwork->GetNumberOfLinks(); ++i )
   {
	   links.push_back( VE_Conductor::GUI_Utilities::Link( this ) );

      links.at( i ).SetFromPort( *(veNetwork->GetLink( i )->GetFromPort()) );
      links.at( i ).SetToPort( *(veNetwork->GetLink( i )->GetToPort()) );

      long moduleID;
      veNetwork->GetLink( i )->GetFromModule()->GetData( moduleID );
      links.at( i ).SetFromModule( moduleID );
      veNetwork->GetLink( i )->GetToModule()->GetData( moduleID );
      links.at( i ).SetToModule( moduleID );

      size_t numberOfPoints = veNetwork->GetLink( i )->GetNumberOfLinkPoints();
      for ( size_t j = 0; j < numberOfPoints; ++j )
      {
         std::pair< unsigned int, unsigned int > rawPoint = veNetwork->GetLink( i )->GetLinkPoint( j )->GetPoint();
         wxPoint point;
         point.x = rawPoint.first;
         point.y = rawPoint.second;
         links.at( i ).SetPoint( &point );
      }
      // Create the polygon for links
      links.at( i ).CalcLinkPoly();
   }
   _fileProgress->Update( 50, "create models" );
   // do this for models
   networkWriter.ReadXMLData( xmlNetwork, "Model", "veModel" );
   objectVector = networkWriter.GetLoadedXMLObjects();

   _fileProgress->Update( 75, "done create models" );
   // now lets create a list of them
   for ( size_t i = 0; i < objectVector.size(); ++i )
   {
      VE_Model::Model* model = dynamic_cast< VE_Model::Model* >( objectVector.at( i ) );

      wxClassInfo* cls = wxClassInfo::FindClass( model->GetModelName().c_str() );
      // If the class has not had a custom module been created
      if ( cls == 0 )
      {
         //Load the generic plugin for conductor
         cls = wxClassInfo::FindClass( "DefaultPlugin" );
      }
      REI_Plugin* tempPlugin = dynamic_cast< REI_Plugin* >( cls->CreateObject() );

      Module temp_mod;
      unsigned int num = model->GetModelID();
	   modules[ num ] = temp_mod;
	   modules[ num ].SetPlugin( tempPlugin );
      modules[ num ].GetPlugin()->SetID( num );
	   modules[ num ].SetClassName( model->GetModelName() );
      modules[ num ].GetPlugin()->SetVEModel( model );
      //Second, calculate the polyes
      wxRect bbox = modules[ num ].GetPlugin()->GetBBox();
      int polynum = modules[ num ].GetPlugin()->GetNumPoly();
      POLY tmpPoly;
      tmpPoly.resize( polynum );
      modules[ num ].GetPlugin()->GetPoly(tmpPoly);
      VE_Conductor::GUI_Utilities::Polygon tempPoly;
      *(tempPoly.GetPolygon()) = tmpPoly;
      tempPoly.TransPoly( bbox.x, bbox.y, *(modules[ num ].GetPolygon()) ); //Make the network recognize its polygon 
//std::cout << " reveiw : " << std::endl
//      << num << " : "<< model->GetModelName() << " : " << bbox.x << " : " << bbox.y << std::endl;
   }
   /*
   // do this for tags
   DOMNodeList* subElements = doc->getDocumentElement()->getElementsByTagName( xercesString("veTag") );
   unsigned int numTags = subElements->getLength();
   // now lets create a list of them
   for ( unsigned int i = 0; i < numCommands; ++i )
   {
      VE_Model::Tag* temp = new VE_Model::Tag( doc );
      temp->SetObjectFromXMLData( dynamic_cast< DOMElement* >( subElements->item(i) ) );
      veTagVector.push_back( temp );
      tags.push_back( TAG );
      tags.back().text = wxString( veTagVector.back()->GetTagText().c_str() );
      tags.back().cons[0].x = veTagVector.back()->GetTagPoint( 0 )->GetPoint().first;
      tags.back().cons[0].y = veTagVector.back()->GetTagPoint( 0 )->GetPoint().second;
      tags.back().cons[1].x = veTagVector.back()->GetTagPoint( 1 )->GetPoint().first;
      tags.back().cons[1].y = veTagVector.back()->GetTagPoint( 1 )->GetPoint().second;
      tags.back().box.x = veTagVector.back()->GetTagPoint( 2 )->GetPoint().first;
      tags.back().box.x = veTagVector.back()->GetTagPoint( 2 )->GetPoint().second;
      // Create the polygon for tags
      tags.back().CalcTagPoly();
   }
   */
   m_selMod = -1;
   m_selFrPort = -1; 
   m_selToPort = -1; 
   m_selLink = -1; 
   m_selLinkCon = -1; 
   m_selTag = -1; 
   m_selTagCon = -1; 
   xold = yold =0;
   _fileProgress->Update( 100, "Done" );
   //while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR){ ; }
   Refresh();
   delete _fileProgress;
}

//////////////////////////////////////////////////////
void Network::OnShowLinkContent(wxCommandEvent& WXUNUSED(event))
{
   char *linkresult;
   //The to Mod are actually the from module for the data flow
   int mod = links[ m_selLink ].GetFromModule(); 
   int port = links[ m_selLink ].GetFromPort();

   try 
   {
      linkresult = exec->GetExportData(mod, port);
   }
   catch ( CORBA::Exception& ) 
   {
      std::cerr << "Maybe Engine is down" << std::endl;
      return;
   }

   if ( std::string(linkresult) !=std::string(""))
   {
      Package p;
      p.SetSysId("linkresult.xml");
      p.Load( linkresult, strlen(linkresult) );

      // In the new code this would pass in a datavalue pair
      UIDialog* port_dlg = modules[mod].GetPlugin()->PortData( NULL, &(p.intfs[0]) );

      if ( port_dlg != NULL )
         port_dlg->Show();
   }
}

//////////////////////////////////////////////////////
void  Network::OnShowResult(wxCommandEvent& WXUNUSED(event))
{
   char* result;
  
   if ( m_selMod < 0 )
      return;

   if ( CORBA::is_nil( exec.in() ) )
   {
      std::cerr<<"Not Connected yet!" << std::endl;
      return;
   }
  
   try 
   {
      result = exec->GetModuleResult( m_selMod );
   }
   catch (CORBA::Exception &) 
   {
		std::cerr << "Maybe Computational Engine is down" << std::endl;
      return;
   }

   if ( std::string(result) != "" )
   {
      Package p;
      p.SetSysId("linkresult.xml");
      p.Load(result, strlen(result));

      // In the new code this would pass in a datavalue pair
      modules[ m_selMod ].GetPlugin()->UnPackResult( &p.GetInterfaceVector()[0] );
      UIDialog* hello = modules[m_selMod].GetPlugin()->Result(NULL);
      
      if ( hello != NULL )
	      hello->Show();
   }
}

// EPRI TAG
//////////////////////////////////////////////////////
void  Network::OnShowFinancial(wxCommandEvent& WXUNUSED(event))
{
   if (m_selMod<0) 
      return;
   modules[m_selMod].GetPlugin()->FinancialData();
}

//////////////////////////////////////////////////////
void Network::OnShowDesc(wxCommandEvent& WXUNUSED(event))
{
   wxString desc;
   wxString title;
 
   title << wxT("Description");
  
   if (m_selMod<0)
      return;
  
   desc = modules[m_selMod].GetPlugin()->GetDesc();
  
   wxMessageDialog(this, desc, title).ShowModal();
}

void Network::OnParaView(wxCommandEvent& WXUNUSED(event))
{
   //wxArrayString output;
   // ::wxExecute("paraview", wxEXEC_ASYNC|wxEXEC_MAKE_GROUP_LEADER);
   //::wxShell("paraview");
#ifndef WIN32
   paraThread* para_t=new paraThread(this);
   para_t->Create();
   para_t->Run();
#else
   ::wxExecute("paraview", wxEXEC_ASYNC);
#endif

}
///////////////////////////////////////////
void Network::OnInputsWindow(wxCommandEvent& WXUNUSED(event))
{
   if (m_selMod<0) 
      return;
   // Here we launch a dialog for a specific plugins input values
   modules[m_selMod].GetPlugin()->ViewInputVariables();
}
///////////////////////////////////////////
void Network::OnResultsWindow(wxCommandEvent& WXUNUSED(event))
{
   if (m_selMod<0) 
      return;
   // Here we launch a dialog for a specific plugins input values
   modules[m_selMod].GetPlugin()->ViewResultsVariables();
}
///////////////////////////////////////////
void Network::OnGeometry(wxCommandEvent& WXUNUSED(event))
{
   if ( !SetActiveModel() ) 
   {
      return;
   }

   // Here we launch a dialog for a specific plugins input values
   VE_Model::Model* veModel = modules[m_selMod].GetPlugin()->GetModel();

   if( !cadDialog )
   {
      cadDialog = new VE_Conductor::GUI_Utilities::CADNodeManagerDlg( veModel->AddGeometry(),
                                                               this, ::wxNewId() );

      cadDialog->SetSize(dynamic_cast<AppFrame*>(wxTheApp->GetTopWindow())->GetAppropriateSubDialogSize());
   }

   cadDialog->SetVjObsPtr( xplorerPtr.in() );
   cadDialog->ShowModal();
   // Get cadnode back
   *( dynamic_cast< VE_CAD::CADAssembly* >( veModel->GetGeometry() ) ) = 
      *( dynamic_cast< VE_CAD::CADAssembly* >( cadDialog->GetRootCADNode() ) );
}
///////////////////////////////////////////
void Network::OnDataSet( wxCommandEvent& WXUNUSED(event) )
{
   if ( !SetActiveModel() ) 
   {
      return;
   }
   
   // Here we launch a dialog for a specific plugins input values
   VE_Model::Model* veModel = modules[m_selMod].GetPlugin()->GetModel();
   //DataSetLoaderUI* dataSetLoaderDlg = 0;
   /*if ( CORBA::is_nil( xplorerPtr.in() ) )
   {
      ((AppFrame*)(parent->GetParent()->GetParent()))->ConVEServer();
      SetXplorerInterface( ((AppFrame*)(parent->GetParent()->GetParent()))->GetXplorerObject() );
      if ( CORBA::is_nil( xplorerPtr.in() ) )
         return;
   }*/
   /*dataSetLoaderDlg = new DataSetLoaderUI( this, ::wxNewId(), 
               SYMBOL_DATASETLOADERUI_TITLE, SYMBOL_DATASETLOADERUI_POSITION, 
               SYMBOL_DATASETLOADERUI_SIZE, SYMBOL_DATASETLOADERUI_STYLE, veModel );*/
   DataSetLoaderUI dataSetLoaderDlg( this, ::wxNewId(), 
               SYMBOL_DATASETLOADERUI_TITLE, SYMBOL_DATASETLOADERUI_POSITION, 
               SYMBOL_DATASETLOADERUI_SIZE, SYMBOL_DATASETLOADERUI_STYLE, veModel );

   //cadDialog->SetVjObsPtr( xplorerPtr.in() );
   if ( dataSetLoaderDlg.ShowModal() == wxID_OK )
   {
      //Now send the data to xplorer
      VE_XML::XMLReaderWriter netowrkWriter;
      netowrkWriter.UseStandaloneDOMDocumentManager();

      // Create the command and data value pairs
      VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair();
      dataValuePair->SetData( "CREATE_NEW_DATASETS", veModel );
      VE_XML::Command* veCommand = new VE_XML::Command();
      veCommand->SetCommandName( std::string("UPDATE_MODEL_DATASETS") );
      veCommand->AddDataValuePair( dataValuePair );

      dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->SendCommandStringToXplorer( veCommand );

      //Clean up memory
      delete veCommand;
      veCommand = 0;
   }

  // delete dataSetLoaderDlg;
   //dataSetLoaderDlg = 0;
}
///////////////////////////////////////////
void Network::OnVisualization(wxCommandEvent& WXUNUSED(event))
{
   if ( !SetActiveModel() ) 
   {
      return;
   }
  
   //Get the active model ID from the xml data
   VE_Model::Model* activeXMLModel = modules[m_selMod].GetPlugin()->GetModel();
   unsigned int modelID = activeXMLModel->GetModelID();

   //Get the active model from the CORBA side
   ///Should this be a member variable?
  // VjObs::Model_var activeCORBAModel; 
   VjObs::Model* activeCORBAModel; 

   //Does this need to be wrapped in something else?
   if ( !CORBA::is_nil( xplorerPtr.in() ) )
   {
      try
      {
         if(xplorerPtr->GetModel(modelID))
         {
            activeCORBAModel = xplorerPtr->GetModel(modelID);
            if(!vistab)
            {
               vistab = new Vistab (activeCORBAModel,this,
                                SYMBOL_VISTAB_IDNAME,
                                activeXMLModel->GetModelName().c_str(),
                                SYMBOL_VISTAB_POSITION,
                                SYMBOL_VISTAB_SIZE,
                                SYMBOL_VISTAB_STYLE );
            }
            else
            {
               vistab->SetActiveModel(activeCORBAModel);
            }
            vistab->SetCommInstance(xplorerPtr);
            size_t nInformationPackets = activeXMLModel->GetNumberOfInformationPackets();
            if(nInformationPackets)
            {
               wxArrayString scalarTextureDatasets;
               wxArrayString vectorTextureDatasets;
               bool hasScalarTextures = false;
               bool hasVectorTextures = false;
               for(size_t i = 0; i < nInformationPackets; i++)
               {
                  VE_XML::ParameterBlock* paramBlock = activeXMLModel->GetInformationPacket(i);
                  size_t numProperties = paramBlock->GetNumberOfProperties();
                  
                  for ( size_t i = 0; i < numProperties; ++i )
                  {
                     VE_XML::DataValuePair* dataValuePair = paramBlock->GetProperty( i );
                     if ( dataValuePair->GetDataName() == "VTK_TEXTURE_DIR_PATH" )
                     {
                        
                        size_t textureDataType = dataValuePair->GetDataString().find("scalars");
                        if(textureDataType < dataValuePair->GetDataString().size())
                        {
                           scalarTextureDatasets.Add(dataValuePair->GetDataString().c_str());
                           hasScalarTextures = true;
                        }
                        else 
                        {
                           textureDataType = dataValuePair->GetDataString().find("vectors");
                           if(textureDataType < dataValuePair->GetDataString().size())
                           {
                               vectorTextureDatasets.Add(dataValuePair->GetDataString().c_str());
                               hasVectorTextures = true;
                           }
                        }
                     }
                  }
               }
               if(hasScalarTextures)
               {
                  std::cout<<"Found scalar texture directory"<<std::endl;
                  vistab->SetTextureData(scalarTextureDatasets,"TEXTURE_SCALARS");
               }
               if(hasVectorTextures)
               {
                  std::cout<<"Found vector texture directory"<<std::endl;
                  vistab->SetTextureData(vectorTextureDatasets,"TEXTURE_VECTORS");
               }

            }
            int error = vistab->ShowModal(); 
         }
         else
         { 
            std::cout << " Model contains no datasets: " << modelID<<std::endl;
            return;
         }
      }
      catch ( CORBA::Exception& )
      {
         std::cout << " Couldn't find model: " << modelID<<std::endl;
         return;
      }
   }
   else
   {
      std::cerr << " ERROR : Not connected to VE-Server " << std::endl;
      return;
   }
}
///////////////////////////////////////////
std::pair< double, double >* Network::GetUserScale( void )
{
   return &userScale;
}
///////////////////////////////////////////
std::pair< unsigned int, unsigned int >* Network::GetNumPix( void )
{
   return &numPix;
}
///////////////////////////////////////////
std::pair< unsigned int, unsigned int >* Network::GetNumUnit( void )
{
   return &numUnit;
}
///////////////////////////////////////////
wxPoint Network::GetPointForSelectedPlugin( unsigned long moduleID, unsigned int portNumber, std::string portType )
{
   wxRect bbox = modules[ moduleID ].GetPlugin()->GetBBox();
   int num = 0;
   PORT ports;
   wxPoint tempPoint;

   if ( portType == "input" )
   {
      num = modules[ moduleID ].GetPlugin()->GetNumIports();	
      ports.resize(num);
      modules[ moduleID ].GetPlugin()->GetIPorts( ports );
   }
   else if ( portType == "output" )
   {
      num = modules[ moduleID ].GetPlugin()->GetNumOports();	
      ports.resize(num);
      modules[ moduleID ].GetPlugin()->GetOPorts( ports );
   }
   else
   {
      std::cerr << "ERROR: The proper port type was not specified (input or output)" << std::endl;
      return tempPoint;
   }

   if ( num > 0 )
   {
      size_t index = 0;
      for ( size_t i = 0; i < ports.size(); ++i )
      {
         if ( ports.at( i ).GetPortNumber() == portNumber )
         {
            index = i;
            break;
         }
      }
      wxPoint portPoint( ports[ index ].GetPortLocation()->GetPoint().first, ports[ index ].GetPortLocation()->GetPoint().second );
      tempPoint.x = bbox.x+portPoint.x;
      tempPoint.y = bbox.y+portPoint.y;
   }

   return tempPoint;
}
///////////////////////////////////////////
void* Network::Entry( void )
{
   isLoading = true;
   this->CreateNetwork( tempXMLNetworkData );
   isLoading = false;
   return 0;
}
///////////////////////////////////////////
bool Network::SetActiveModel( void )
{
   if (m_selMod<0) 
      return false;

   //std::cout << m_selMod << std::endl;
   // Create the command and data value pairs
   VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair(  std::string("UNSIGNED INT") );
   dataValuePair->SetDataName( "CHANGE_ACTIVE_MODEL" );
   dataValuePair->SetDataValue( static_cast< unsigned int >( m_selMod ) );
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName( std::string("CHANGE_ACTIVE_MODEL") );
   veCommand->AddDataValuePair( dataValuePair );

   bool connected = dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->SendCommandStringToXplorer( veCommand );

   if ( connected )
   {
      SetXplorerInterface( dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->GetXplorerPointer() );
   }

   //Clean up memory
   delete veCommand;
   return connected;
}
