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
 * File:          $RCSfile: Network.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/Network.h"
#include "VE_Conductor/Framework/PortDialog.h"
#include "VE_Conductor/Framework/package.h"
#include "VE_Conductor/Framework/paraThread.h"
#include "VE_Conductor/Framework/Geometry.h"
#include "VE_Conductor/Framework/UIDialog.h"
#include "VE_Conductor/Framework/GlobalParamDialog.h"

#include <wx/dc.h>
#include <wx/dcbuffer.h>

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
   EVT_MENU(SHOW_FINANCIAL, Network::OnShowFinancial) /* EPRI TAG */
   EVT_MENU(GEOMETRY, Network::OnGeometry) /* EPRI TAG */
END_EVENT_TABLE()

Network::Network(wxWindow* parent, int id)
  :wxScrolledWindow(parent, id, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL | wxNO_FULL_REPAINT_ON_RESIZE)
{
   modules.clear();
   links.clear();
   m_xUserScale=1;
   m_yUserScale=1;
   nUnitX=100;
   nUnitY=240;
   nPixX = 10;
   nPixY = 10;
   SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
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

   SetBackgroundColour(*wxWHITE);
}

Network::~Network()
{
   links.clear();

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
   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);

   wxPaintDC dc(this);
   PrepareDC(dc);
  
   dc.SetUserScale( m_xUserScale, m_yUserScale );
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
  POLY ports;
 	
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );

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
	      if (computenorm(temp, ports[i])<=10)
	      {
	         m_selFrPort = i;
	         break;
	      }
  
      ports.resize( modules[m_selMod].GetPlugin()->GetNumOports() );
	   modules[m_selMod].GetPlugin()->GetOPorts( ports );
      for ( unsigned int i=0; i<ports.size(); i++)
	      if (computenorm(temp, ports[i])<=10)
	      {
	         m_selToPort = i;
	         break;
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
  dc.SetUserScale( m_xUserScale, m_yUserScale );
  
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
   dc.SetUserScale( m_xUserScale, m_yUserScale );

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
      DropModule(x, y, m_selMod, dc);
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
   dc.SetUserScale( m_xUserScale, m_yUserScale );

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
  dc.SetUserScale( m_xUserScale, m_yUserScale );

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
  pop_menu.Append(SHOW_FINANCIAL, "Financial Data");
  pop_menu.Enable(SHOW_FINANCIAL, true);

   // GUI to configure geometry for graphical env
  pop_menu.Append(GEOMETRY, "Geometry Config");
  pop_menu.Enable(GEOMETRY, true);

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
    };

  if (m_selTag>=0 )
    {
      pop_menu.Enable(EDIT_TAG, true);
      pop_menu.Enable(DEL_TAG, true);
    };
  
  if (m_selMod>=0)
    {
      pop_menu.Enable(DEL_MOD, true);
      pop_menu.Enable(SHOW_RESULT, true);
      if (modules[m_selMod].GetPlugin()->Has3Ddata())
	pop_menu.Enable(PARAVIEW, true);
    };
    
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
   VE_Conductor::GUI_Utilities::Polygon linkline, Near;
   wxRect bbox;
   std::vector<wxPoint> ports;  
   int n;
   int num;
  
   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
   links[m_selLink].DrawLink( false );

   //n = links[m_selLink].GetNumberOfPoints()+2;
   //linkline.resize(n);

   unsigned long fromModuleID = links[m_selLink].GetFromModule();
   bbox = modules[ fromModuleID ].GetPlugin()->GetBBox();  
   num = modules[ fromModuleID ].GetPlugin()->GetNumOports();
   ports.resize(num);
   modules[ fromModuleID ].GetPlugin()->GetOPorts(ports);

   unsigned int fromPort = links[m_selLink].GetFromPort();
   linkline.GetPoint( 0 )->x = bbox.x+ports[ fromPort ].x;
   linkline.GetPoint( 0 )->y = bbox.y+ports[ fromPort ].y;

  size_t i;
  for ( i=0; i< links[m_selLink].GetPoints()->size(); i++ )
    *(linkline.GetPoint( i+1 )) = *(links[ m_selLink ].GetPoint( i ));
   
   unsigned long toModuleID = links[m_selLink].GetToModule();
   bbox = modules[ toModuleID ].GetPlugin()->GetBBox();
   num = modules[ toModuleID ].GetPlugin()->GetNumIports();	
   ports.resize(num);
   modules[ toModuleID ].GetPlugin()->GetIPorts(ports);

   unsigned int toPort = links[m_selLink].GetToPort();
   linkline.GetPoint( n-1 )->x = bbox.x+ports[ toPort ].x;
   linkline.GetPoint( n-1 )->y = bbox.y+ports[ toPort ].y;
  
   linkline.nearpnt( action_point, Near );

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
  links[m_selLink].DrawLinkCon( true );
  m_selLink = -1;
  m_selLinkCon = -1;
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
    
}
/////////////////////////////////////////////////////
void Network::OnEditTag(wxCommandEvent& WXUNUSED(event))
{
   wxClientDC dc(this);
   PrepareDC(dc);

   dc.SetUserScale( m_xUserScale, m_yUserScale );

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
  std::vector< Link >::iterator iter;
  std::vector< Link >::iterator iter2;
  int i;

  int answer=wxMessageBox("Do you really want to delete this link?", "Confirmation", wxYES_NO);
  if (answer!=wxYES)
    return;
  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);

   std::map< int, Module >::iterator miter;
   for (miter=modules.begin(); miter!=modules.end(); miter++)
   {
      i=miter->first;
      unsigned int j;
      for (iter2=modules[i].GetLinks()->begin(), j=0; j<modules[i].GetLinks()->size(); iter2++, j++)
	      if ( modules[i].GetLink( j ) == &(links[ m_selLink ]) )
	      {
	         modules[i].RemoveLink( j );
	         break;
	      }
   }

   for (iter=links.begin(), i=0; iter!=links.end(); iter++, i++)
      if (i==m_selLink)
      {
	      links.erase(iter);
	      m_selLink=-1;
	      break;
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
   
   links[m_selLink].DrawLinkCon( false );

   for (iter=links[m_selLink].GetPoints()->begin(), i=0; iter!=links[m_selLink].GetPoints()->end(); iter++, i++)
      if ( i == m_selLinkCon )
      {
         links[m_selLink].GetPoints()->erase(iter);
         links[m_selLink].CalcLinkPoly();
         m_selLinkCon=-1;
         break;
      }

   links[m_selLink].DrawLinkCon( true );

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
   ReDrawAll();
}

/////////////////////////////////////////////////////
void Network::OnDelMod(wxCommandEvent& WXUNUSED(event))
{
   std::map< int, Module >::iterator iter;
   std::vector< Link >::iterator iter2;
   std::vector< Link >::iterator iter3;
   Link del_link( this );

   int answer=wxMessageBox("Do you really want to delete this module?", "Confirmation", wxYES_NO);
   if (answer!=wxYES)
      return;

   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);

   //first, delete all the links connects to it
   while ( modules[m_selMod].GetNumberOfLinks() > 0 )
   {
      del_link = *(modules[m_selMod].GetLink( 0 ));

      int k;
      for ( k = 0; k < modules[ del_link.GetToModule() ].GetNumberOfLinks(); ++k )
      {
         Link* tempLink = modules[ del_link.GetToModule() ].GetLink( k );
         if ( (tempLink->GetToPort() == del_link.GetToPort()) &&
               (del_link.GetFromModule() == tempLink->GetFromModule())
            )
         {
		      modules[ del_link.GetToModule() ].RemoveLink( k );
		      break;
         }
      }

      for (iter3=links.begin(), k=0; iter3!=links.end(); iter3++, k++)
         if ( (links[ k ].GetToPort() == del_link.GetToPort()) &&
               (del_link.GetFromModule() == links[ k ].GetFromModule()) &&
               (del_link.GetToModule() == links[ k ].GetToModule()) &&
               (del_link.GetFromPort() == links[ k ].GetFromPort())
            )
         {
	         links.erase(iter3);
	         break;
	      } 
   }

   for (iter=modules.begin(); iter!=modules.end(); iter++)
      if (iter->first==m_selMod)
      {
	      delete modules[m_selMod].GetPlugin();
	      //modules[m_selMod].GetPlugin() =NULL;
	      modules.erase(iter);
	      m_selLink=-1;
	      break;
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
	         return i;
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
	      links[i].DrawLinkCon( true ); //draw link connectors
	      m_selLink = i;
	      return i;
	   }
   }
   return -1;
}

//////////////////////////////////////////////////////
void Network::UnSelectLink(wxDC &dc)
{
   links[m_selLink].DrawLinkCon( false );//wipe link connectors
   
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
	      tags[i].DrawTagCon( true );
	      m_selTag = i;
	      return i;
	   }
   }
   return -1;
}

/////////////////////////////////////////////////
void Network::UnSelectTag(wxDC &dc)
{
  tags[m_selTag].DrawTagCon( false );
  
  ReDraw(dc);
  m_selTag = -1;
  return;
}

/////////////////////////////////////////////////
/////////////// Misc Functions //////////////////
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
  int i;
  bool scroll = false; 

  GetScrollPixelsPerUnit(&xunit, &yunit);
  GetViewStart(&xpos, &ypos);
  GetVirtualSize(&sx, &sy);
  GetClientSize(&w, &h);

  xpos = (int)( 1.0 * xpos / m_xUserScale );
  ypos = (int)( 1.0 * ypos / m_xUserScale );
  oldxpos = xpos;
  oldypos = ypos;

  sx = (int)( 1.0*sx / m_xUserScale );
  sy = (int)( 1.0*sy / m_xUserScale );
  w = (int)( 1.0*w / m_xUserScale );
  h = (int)( 1.0*h / m_xUserScale ); 
  
  ex=xpos*xunit+w;
  ey=ypos*yunit+h;

  if (mod < 0) // no module is selected
    return; 
  
  cur_module = modules[mod].GetPlugin();
      
  bbox = cur_module->GetBBox(); //Get the Boundoing box of the modul 
  
  if (x>ex-bbox.width)
    xpos+=1;//m_xUserScale;
  else  if (x<(ex-w)+relative_pt.x)
    xpos-=1;//m_xUserScale;
  
  if (y>ey-bbox.height)
    ypos+=1;//m_xUserScale;
  if (y<(ey-h+relative_pt.y))
    ypos-=1;//m_xUserScale;
  
  if (x-relative_pt.x+bbox.width > sx)
    {
      nUnitX+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }
  if (y-relative_pt.y+bbox.height > sy)
    {
      nUnitY+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }

  //wipe off the old link connects with this module
  for ( i=0; i < modules[mod].GetNumberOfLinks(); i++)
    modules[mod].GetLink( i )->DrawLink( false );
    
  cur_module->SetPos(wxPoint(x-relative_pt.x, y-relative_pt.y));
  
  if ((bbox.x-3.0/m_xUserScale)>0)
    bbox.x-=(int)( 3.0/m_xUserScale );
  else
    bbox.x=0;
  
  if ((bbox.y-3.0/m_xUserScale)>0)
    bbox.y-=(int)( 3.0/m_xUserScale );
  else
    bbox.y=0;
  
  bbox.width+=(int)( 3.0/m_xUserScale );
  bbox.height+=(int)( 3.0/m_xUserScale );

  CleanRect(bbox, dc);  
  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = (int)( 1.0 * xpos * m_xUserScale );
      ypos = (int)( 1.0 * ypos * m_xUserScale );
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
 
}

/////////////////////////////////////////////////////////////
void Network::DropModule(int ix, int iy, int mod, wxDC& dc)
{
  wxRect bbox; //Bounding box  
  int sx, sy;
  double r;
  int vx, vy;
  int x, y;
  int i, num;
  REI_Plugin * cur_module;
  bool scroll = false; 

  //In drag mode
  if (mod < 0) // no module is selected
    return; 
  
  GetVirtualSize(&sx, &sy);
  sx = (int)( 1.0*sx / m_xUserScale );
  sy = (int)( 1.0*sy / m_xUserScale );
  
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
  //  vx= vx / m_xUserScale;
  //  vy = vy / m_xUserScale;
  
  if (x-relative_pt.x+bbox.width > sx)
    {
      r=(1.0*(x-relative_pt.x+bbox.width) / sx);
      nUnitX=int (r*nUnitX+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vx = nUnitX;
    }
  if (y-relative_pt.y+bbox.height > sy)
    {
      r=(1.0*(y-relative_pt.y+bbox.width) / sy);
      nUnitY=int (r*nUnitY+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vy = nUnitY;
    }
  
  cur_module->SetPos(wxPoint(x-relative_pt.x, y-relative_pt.y));

  //num = cur_module->GetNumPoly();
  //tmppoly.resize(num);
  VE_Conductor::GUI_Utilities::Polygon tmppoly;
   POLY oldPoly;
  cur_module->GetPoly( oldPoly );
   *(tmppoly.GetPolygon()) = oldPoly;
  tmppoly.TransPoly( x-relative_pt.x, y-relative_pt.y, *(modules[mod].GetPolygon()) );

  //Recalce links poly as well
  for (i=0; i<(int)modules[mod].GetNumberOfLinks(); i++)
    modules[mod].GetLink( i )->CalcLinkPoly();

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

  //xpos = 1.0 * xpos * m_xUserScale;
  //ypos = 1.0 * ypos * m_xUserScale;
      
  //  Scroll(vx, vy);  
  //CleanRect(bbox, dc);
  ReDrawAll();

}

/////////////////////////////////////////////////////////////////////////
void Network::TryLink(int x, int y, int mod, int pt, wxDC& dc, bool flag)
{
   int xoff, yoff;
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

   if (flag)
   {
      DrawPorti(modules[mod].GetPlugin(), pt, flag);
      ports.resize( modules[mod].GetPlugin()->GetNumIports() );
      modules[mod].GetPlugin()->GetIPorts(ports);

      bbox = modules[mod].GetPlugin()->GetBBox();
      xoff = ports[pt].x+bbox.x;
      yoff = ports[pt].y+bbox.y;
   }
   else
   {
      DrawPorti(modules[mod].GetPlugin(), pt, flag);
      ports.resize( modules[mod].GetPlugin()->GetNumOports() );
      modules[mod].GetPlugin()->GetOPorts(ports);

      bbox = modules[mod].GetPlugin()->GetBBox();
      xoff = ports[pt].x+bbox.x;
      yoff = ports[pt].y+bbox.y;      
   }

   dc.SetPen(*wxWHITE_PEN);
   dc.DrawLine(xoff, yoff, xold, yold);
   ReDraw(dc);

   if ( dest_mod >=0 )
      DrawPorts( modules[dest_mod].GetPlugin(), true); //draw the ports

   dc.SetPen(*wxBLACK_PEN);
   dc.DrawLine(xoff, yoff, x, y);

   xold = x;
   yold = y;
}

////////////////////////////////////////////////////////////////////////
void Network::DropLink(int x, int y, int mod, int pt, wxDC &dc, bool flag)
{
   //first check if there is an apropriate port on the destination position
   //in the mean time, also find out the wipe off line's start position 
   int xoff, yoff;

   POLY ports;
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
   if ( flag )
   {
      DrawPorts(modules[mod].GetPlugin(), false); //Wipe off the port rect

      ports.resize( modules[mod].GetPlugin()->GetNumIports() );
      modules[mod].GetPlugin()->GetIPorts(ports);

      bbox = modules[mod].GetPlugin()->GetBBox();
      xoff = ports[pt].x+bbox.x;
      yoff = ports[pt].y+bbox.y;

      if (dest_mod>=0)
      {
         ports.resize( modules[dest_mod].GetPlugin()->GetNumOports() );
         modules[dest_mod].GetPlugin()->GetOPorts(ports);
   
         for (i=0; i<(int)ports.size(); i++)
            if (computenorm(temp, ports[i])<=10) 
            {
               dest_port = i;
               break;
            }
      }
   }
   else    // If ouput port
   {
      DrawPorts(modules[mod].GetPlugin(), false); //Wipe off the port rect

      ports.resize( modules[mod].GetPlugin()->GetNumOports() );
      modules[mod].GetPlugin()->GetOPorts(ports);
      bbox = modules[mod].GetPlugin()->GetBBox();
      xoff = ports[pt].x+bbox.x;
      yoff = ports[pt].y+bbox.y;     

      // check if the drop point is a out port
      if (dest_mod>=0)
      {
         ports.resize( modules[dest_mod].GetPlugin()->GetNumIports() );
         modules[dest_mod].GetPlugin()->GetIPorts(ports);
         for (i=0; i<(int)ports.size(); i++)
            if (computenorm(temp, ports[i])<=10)
            {
               dest_port = i;
               break;
            }
      }
   }  

   //Wipe off the test line
   dc.SetPen(*wxWHITE_PEN);
   dc.DrawLine(xoff, yoff, xold, yold);

   // if it is a good link
   if (dest_mod>=0 && dest_port>=0 && (dest_mod!=mod||dest_port!=pt))
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
      for (i=0; i<(int)modules[mod].GetNumberOfLinks(); i++)
      {
         Link* tempLink = modules[mod].GetLink( i );
         if ( (tempLink->GetToModule() == ln.GetToModule() )
               && (tempLink->GetToPort() == ln.GetToPort() )
               && (tempLink->GetFromModule() == ln.GetFromModule() )
               && (tempLink->GetFromPort() == ln.GetFromPort() )
            )
         {
            {
               // Get input port point for the link
               wxRect bbox = modules[ ln.GetFromModule() ].GetPlugin()->GetBBox();
               int num = modules[ ln.GetFromModule() ].GetPlugin()->GetNumOports();
               POLY ports;
               ports.resize( num );
               modules[ ln.GetFromModule() ].GetPlugin()->GetOPorts(ports);
               // get initial port
               wxPoint pos;
               pos.x = bbox.x+ports[ ln.GetFromPort() ].x;
               pos.y = bbox.y+ports[ ln.GetFromPort() ].y;
               ln.GetPoints()->push_back( pos );
            }

            {
               // Get input port point for the link
               wxRect bbox = modules[ ln.GetToModule() ].GetPlugin()->GetBBox();  
               int num = modules[ ln.GetToModule() ].GetPlugin()->GetNumIports();
               POLY ports;
               ports.resize( num );
               modules[ ln.GetToModule() ].GetPlugin()->GetIPorts( ports );
               wxPoint pos;
               pos.x = bbox.x+ports[ ln.GetToPort() ].x;
               pos.y = bbox.y+ports[ ln.GetToPort() ].y;
               ln.GetPoints()->push_back( pos );
            }

            found = true;
         }
      }

      if ( !found ) // no duplicate links are allowed
      { 
         ln.CalcLinkPoly();
         links.push_back( ln );
         modules[ mod ].GetLinks()->push_back( ln ); //push_back the index of the link
         modules[ dest_mod ].GetLinks()->push_back( ln );
      }
   }

   m_selMod = -1;
   m_selFrPort = -1;
   m_selToPort = -1;
   ReDrawAll();
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

  sx = (int)( 1.0*sx / m_xUserScale );
  sy = (int)( 1.0*sy / m_xUserScale );
  w = (int)( 1.0*w / m_xUserScale );
  h = (int)( 1.0*h / m_xUserScale ); 

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
      nUnitX+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }
  if (y > sy)
    {
      nUnitY+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }

  //erase the original link;
  links[ln].DrawLink( false );
  links[ln].DrawLinkCon( false );
  *(links[ln].GetPoint( ln_con )) = wxPoint(x,y);
 
   if ( oldxpos!=xpos || oldypos!=ypos || scroll)
   {
      xpos = (int)( 1.0 * xpos * m_xUserScale );
      ypos = (int)( 1.0 * ypos * m_xUserScale );

      Scroll(xpos, ypos);
      ReDrawAll();
   }
   else
      ReDraw(dc);

   links[ln].DrawLinkCon( true );
}

//////////////////////////////////////////////////////////////////////
void Network::DropLinkCon(int x, int y, int ln, int ln_con, wxDC &dc)
{
  int sx, sy;
  double r;
  int vx, vy;
  bool scroll = false; 
  
  GetVirtualSize(&sx, &sy);

  sx = (int)( 1.0*sx / m_xUserScale );
  sy = (int)( 1.0*sy / m_xUserScale );
  //  w = w / m_xUserScale;
  //h = h / m_xUserScale; 
      
   GetViewStart(&vx,&vy);
   if (x > sx)
   {
      r=(1.0*x/sx);
      nUnitX=int (r*nUnitX+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vx = nUnitX;
   }

   if (y > sy)
   {
      r=(1.0*y/sy);
      nUnitY=int (r*nUnitY+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vy = nUnitY;
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
  
  sx = (int)( 1.0*sx / m_xUserScale );
  sy = (int)( 1.0*sy / m_xUserScale );
  w = (int)( 1.0*w / m_xUserScale );
  h = (int)( 1.0*h / m_xUserScale ); 

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
      nUnitX+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }
  if (y > sy)
    {
      nUnitY+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }

  //erase the original Tag;
  tags[t].DrawTag( false );
  tags[t].DrawTagCon( false );
  *(tags[t].GetConnectorsPoint( t_con ))=wxPoint(x,y);
  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = (int)( 1.0 * xpos * m_xUserScale );
      ypos = (int)( 1.0 * ypos * m_xUserScale );
      
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
  tags[t].DrawTagCon( true );
  
}

//////////////////////////////////////////////////////////////////
void Network::DropTagCon(int x, int y, int t, int t_con, wxDC &dc)
{
  int sx, sy;
  double r;
  int vx, vy;
  
  bool scroll = false; 

  GetVirtualSize(&sx, &sy);
 
  sx = (int)( 1.0*sx / m_xUserScale );
  sy = (int)( 1.0*sy / m_xUserScale );
  //  w = w / m_xUserScale;
  //  h = h / m_xUserScale; 
     
  GetViewStart(&vx,&vy);
  if (x > sx)
    {
      r=(1.0*x/sx);
      nUnitX=int (r*nUnitX+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      vx = nUnitX;
      scroll = true;
    }
  if (y > sy)
    {
      r=(1.0*y/sy);
      nUnitY=int (r*nUnitY+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vy = nUnitY;
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

  sx = (int)( 1.0*sx / m_xUserScale );
  sy = (int)( 1.0*sy / m_xUserScale );
  w = (int)( 1.0*w / m_xUserScale );
  h = (int)( 1.0*h / m_xUserScale ); 

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
      nUnitX+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }
  if (y > sy)
    {
      nUnitY+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }

  //erase the original Tag;
  tags[t].DrawTag( false );
  tags[t].DrawTagCon( false );

  tags[t].GetBoundingBox()->x = x-tag_rpt.x;
  tags[t].GetBoundingBox()->y = y-tag_rpt.y;

  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = (int)( 1.0 * xpos * m_xUserScale );
      ypos = (int)( 1.0 * ypos * m_xUserScale );
      
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
  tags[t].DrawTagCon( true );
  
}

/////////////////////////////////////////////////////
void Network::DropTag(int x, int y, int t, wxDC &dc)
{
  int sx, sy;
  double r;
  int vx, vy;
  bool scroll = false; 
  
  GetVirtualSize(&sx, &sy);

  sx = (int)(1.0*sx / m_xUserScale);
  sy = (int)(1.0*sy / m_xUserScale);
  //  w = w / m_xUserScale;
  //  h = h / m_xUserScale; 
      
  GetViewStart(&vx,&vy);
  if (x > sx)
    {
      r=(1.0*x/sx);
      nUnitX=int (r*nUnitX+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vx = nUnitX;
    }
  if (y > sy)
    {
      r=(1.0*y/sy);
      nUnitY=int (r*nUnitY+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vy = nUnitY;
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
   dc.SetUserScale( m_xUserScale, m_yUserScale );

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
  dc.SetUserScale( m_xUserScale, m_yUserScale );
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
  dc.SetUserScale(m_xUserScale, m_yUserScale);
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
   //  dc.Clear();
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
      links[i].DrawLink( true );

   // draw all the links
   for ( size_t i = 0; i < tags.size(); ++i )
      tags[i].DrawTag( true );
}

/////////////////////////////////////////////////////////////////////
void Network::DrawPorts( REI_Plugin* cur_module, bool flag )
{
   // flag sets whether we we are erasing the ports or not 
   // This function draws the input and output ports on a selected module
   // that is on the design canvas
   if (!cur_module)
      return;

   POLY ports;
   size_t i;
   wxPoint bport[4];
   wxCoord xoff, yoff;
   int num;

   wxClientDC dc(this);
   PrepareDC(dc);
   dc.SetUserScale( m_xUserScale, m_yUserScale );

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
   }
   else
   {
      dc.SetBrush(*wxWHITE_BRUSH);
      dc.SetPen(*wxWHITE_PEN);
   }

   num = cur_module->GetNumIports();
   ports.resize(num);
   cur_module->GetIPorts(ports);
   for (i=0; i<(int)ports.size(); i++)
   {
      // I believe this means move the points in from the edge of the icon
      // by 3 pixles
      // bbox.x returns the global x location and the ports.x returns the x location with respect to bbox.x
      // the same is also true for the y values 
      xoff = ports[i].x+bbox.x-3;
      yoff = ports[i].y+bbox.y-3;

      // draw the polygon 
      dc.DrawPolygon(4, bport, xoff, yoff);      
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
      xoff = ports[i].x+bbox.x-3;
      yoff = ports[i].y+bbox.y-3;

      dc.DrawPolygon(4, bport, xoff, yoff);      
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
  POLY ports;
  int num;

  if (!cur_module)
    return;
  wxClientDC dc(this);
  wxPoint bport[4];
  wxCoord xoff, yoff;
  wxRect bbox;

  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );

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

  
  xoff = ports[index].x+bbox.x-3;
  yoff = ports[index].y+bbox.y-3;
      
  dc.DrawPolygon(4, bport, xoff, yoff);      
  
  
  dc.SetBrush(old_brush);
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
void Network::Pack(std::vector<Interface> & UIs)
{
  // first record the network global variablables
  Interface ntpk; //the network topology and connection pack
  std::string network_pack;
  //char* vname;
  //module information to be saved
  std::string modCls;
  
  //link information to be saved
  long lnFrMod, lnToMod, lnFrPort, lnToPort;
  std::vector<long> lnConX, lnConY;

  //tag information to be saved
  std::string tagText;
  long tagCon0X, tagCon0Y, tagCon1X, tagCon1Y, tagBoxX, tagBoxY;

  int i,j;
  std::map<int, Module>::iterator iter;

  ntpk._type=0;
  ntpk._category=0;
  ntpk._id=-1;
  ntpk.setVal("m_xUserScale", m_xUserScale);
  ntpk.setVal("m_yUserScale", m_yUserScale);
  ntpk.setVal("nPixX", long(nPixX));
  ntpk.setVal("nPixY", long(nPixY));
  ntpk.setVal("nUnitX", long(nUnitX));
  ntpk.setVal("nUnitY", long(nUnitY));
 
  // second, save the the 3 lists of the modules, links and tags
  ntpk.setVal("Module_size", long(modules.size()));
  
   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      i=iter->first;
      //These are the essential information about a module
      modCls = modules[i].GetClassName();
      //poly can be calculated as mod.poly = TransPoly(cur_module->GetPoly(), bbox.x, bbox.y)
      //links vector can be reconstructed from the link's list
      //The order of modules needs to be preserved for the link list and the module UI
      //the UI information of module is packed in different interface packs

	   std::ostringstream dirStringStream;
	   dirStringStream << "modCls_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), modCls); // this is string
   }

   ntpk.setVal("Link_size", long(links.size()));
   for (i=0; i<(int)links.size(); i++)
   {
      lnFrMod = links[i].GetFromModule();
      lnToMod = links[i].GetToModule();
      lnFrPort = links[i].GetFromPort();
      lnToPort = links[i].GetToPort();

      std::ostringstream dirStringStream;
      dirStringStream << "ln_FrMod_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnFrMod);
      dirStringStream.str("");
      dirStringStream.clear();

	   dirStringStream << "ln_ToMod_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnToMod);
      dirStringStream.str("");
      dirStringStream.clear();

	   dirStringStream << "ln_FrPort_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnFrPort);
      dirStringStream.str("");
      dirStringStream.clear();

	   dirStringStream << "ln_ToPort_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnToPort);
      dirStringStream.str("");
      dirStringStream.clear();
      
      lnConX.clear();
      lnConY.clear();
      //Try to store link cons,
      //link cons are (x,y) wxpoint
      //here I store x in one vector and y in the other
      for (j=0; j<(int)(links[i].GetNumberOfPoints()); j++)
	   {
	      lnConX.push_back( links[i].GetPoint( j )->x );
	      lnConY.push_back( links[i].GetPoint( j )->y );
	   }

	   dirStringStream << "ln_ConX_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnConX);
      dirStringStream.str("");
      dirStringStream.clear();

	   dirStringStream << "ln_ConY_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnConY);
      dirStringStream.str("");
      dirStringStream.clear();
   }

   ntpk.setVal("Tag_size", long(tags.size()));
   for (i=0; i<(int)tags.size(); i++)
   {
      tagText = tags[i].GetTagText()->c_str();
      tagCon0X = tags[i].GetConnectorsPoint( 0 )->x;
      tagCon0Y = tags[i].GetConnectorsPoint( 0 )->y;
      tagCon1X = tags[i].GetConnectorsPoint( 1 )->x;
      tagCon1Y = tags[i].GetConnectorsPoint( 1 )->y;
      tagBoxX = tags[i].GetBoundingBox()->x;
      tagBoxY = tags[i].GetBoundingBox()->y;
	  
      std::ostringstream dirStringStream;
      dirStringStream << "tag_Txt_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagText);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_Con0X_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagCon0X);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_Con0Y_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagCon0Y);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_Con1X_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagCon1X);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_Con1Y_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagCon1Y);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_BoxX_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagBoxX);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_BoxY_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagBoxY);
      dirStringStream.str("");
      dirStringStream.clear();
   }

   UIs.clear();
   UIs.push_back(ntpk);

   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      i=iter->first;
      modules[i].GetPlugin()->SetID(i);
      UIs.push_back(*(modules[i].GetPlugin()->Pack()));
      //if module has geometry data
      // then grab geometry interface
      // call spcific modules geom pack
      if ( modules[i].GetPlugin()->HasGeomInfoPackage() )
      {
         //modules[i].GetPlugin()->GetGeometryInfoPackage()->SetID(i);
         //UIs.push_back( *(modules[i].GetPlugin()->GetGeometryInfoPackage()->Pack()) );
         Geometry* geometry = new Geometry( modules[ i ].GetPlugin()->GetID() );
         geometry->SetGeometryDataBuffer( modules[ i ].GetPlugin()->GetGeometryDataBuffer() );

         UIs.push_back( *(geometry->Pack()));

         delete geometry;
      }
   }

   // Pack up global data
   // This is commented out because the computational
   // engine does not have the capability to handle
   // global data yet. This functionality should
   // be addressed shortly to handle this type of data
   // in the framework.
   //UIs.push_back( *(globalparam_dlg->Pack()) );
}

void Network::UnPack(std::vector<Interface> & intfs)
{
/*   int _id = 0;
   Interface ntpk;
   std::vector<std::string> vars;
   long temp = 0;
   double tempd = 0;
   std::string temps;
   std::vector<long> templ1d;
   int pos, ii, j, num, polynum;
   unsigned int i;
   wxClassInfo * cls;
   wxRect bbox;
   LINK * ln;
   POLY tmpPoly;
   std::map<int, Module>::iterator iter;
   //Read it from the file
   int modsize = 0;
   Module temp_mod;

   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR){;}

   for (i=0; i< links.size(); i++)
   {
      delete links[i];
   }
   links.clear();

   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      i = iter->first;
      delete modules[i].GetPlugin();
   }
   modules.clear();

   tags.clear();

   ntpk = intfs[0];

   vars = ntpk.getInts();
   for (i=0; i<vars.size(); i++)
   {
      ntpk.getVal(vars[i], temp);
      if (vars[i]=="nPixX")
	      nPixX = temp;
      else if (vars[i]=="nPixY")
	      nPixY = temp;
      else if (vars[i]=="nUnitX")
	      nUnitX = temp;
      else if (vars[i]=="nUnitY")
	      nUnitY = temp;
      else if (vars[i]=="Module_size")
	      modsize=temp;
      else if (vars[i]=="Link_size")
	   {
	      links.resize(temp); // repopulate the links vector
	      for (j=0; j<temp; j++)
	      {
	         ln = new LINK;
	         links[j]=ln;
	      }
	   }
      else if (vars[i]=="Tag_size")
	      tags.resize(temp);
      else if ((pos=vars[i].find("ln_FrMod_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+9, 4).c_str());
	      links[num]->Fr_mod=temp;
	   }
      else if ((pos=vars[i].find("ln_ToMod_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+9, 4).c_str());
	      links[num]->To_mod=temp;
	   }
      else if ((pos=vars[i].find("ln_FrPort_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      links[num]->Fr_port=temp;
	   }
      else if ((pos=vars[i].find("ln_ToPort_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      links[num]->To_port=temp;
	   }
      else if ((pos=vars[i].find("tag_Con0X_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      tags[num].cons[0].x = temp;
	   }
      else if ((pos=vars[i].find("tag_Con0Y_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      tags[num].cons[0].y = temp;
	   }
      else if ((pos=vars[i].find("tag_Con1X_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      tags[num].cons[1].x = temp;
	   }
      else if ((pos=vars[i].find("tag_Con1Y_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      tags[num].cons[1].y = temp;
	   }
      else if ((pos=vars[i].find("tag_BoxX_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+9, 4).c_str());
	      tags[num].box.x = temp;
	   }
      else if ((pos=vars[i].find("tag_BoxY_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+9, 4).c_str());
	      tags[num].box.y = temp;
	   }
   }

  vars = ntpk.getDoubles();
  for (i=0; i<vars.size(); i++)
    {
      ntpk.getVal(vars[i], tempd);
      if (vars[i]=="m_xUserScale")
	m_xUserScale = tempd;
      else if (vars[i]=="m_yUserScale")
 	m_yUserScale = tempd;
    }

   vars = ntpk.getStrings();
   for (i=0; i<vars.size(); i++)
   {
      ntpk.getVal(vars[i], temps);
      if ((pos=vars[i].find("modCls_"))!=(int)std::string::npos)
	   {
	      num =atoi(vars[i].substr(pos+7, 4).c_str());
	      cls = wxClassInfo::FindClass(temps.c_str());
	      if (cls==NULL)
	      {
	         // wxMessageBox("Load failed : You don't have that class in your Plugin DLL!", temps.c_str());
	         for (ii=0; ii< (int)links.size(); ii++)
		         if (links[ii]!=NULL)
		            delete links[ii];
	         links.clear();
	      
	         for (iter=modules.begin(); iter!=modules.end(); iter++)
		      {
		         ii = iter->first;
		         if (modules[ii].GetPlugin()!=NULL)
		            delete modules[ii].GetPlugin();
		      }
	         modules.clear();
	      
	         tags.clear();
	         while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
	            return;
	      }

	      modules[num]=temp_mod;
	      modules[num].GetPlugin() = (REI_Plugin *) cls->CreateObject();
         modules[num].GetPlugin()->SetID(num);
	      modules[num].cls_name = temps;
	   }
   
      if ((pos=vars[i].find("tag_Txt_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+8, 4).c_str());
	      tags[num].text = wxString(temps.c_str());
	   }
   }
   
   vars = ntpk.getInts1D();
   for (i=0; i<vars.size(); i++)
   {
      ntpk.getVal(vars[i],templ1d);
      if ((pos=vars[i].find("ln_ConX_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+8, 4).c_str());

	      if ( links[num].GetNumberOfPoints() == 0 )
	         links[num].GetPoints()->resize(templ1d.size());

	      for (j=0; j<(int)templ1d.size(); j++)
	         links[num].GetPoint( j )->x = templ1d[j];
	   }
      else if ((pos=vars[i].find("ln_ConY_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+8, 4).c_str());
	      for (j=0; j<(int)templ1d.size(); j++)
	         links[num].GetPoint( j )->y = templ1d[j];
	   }
   }

   // unpack the modules' UIs
   // start from 1 because the link interface is the first one
   for(i = 1; i<intfssize; ++i)
   {
      
      _id = intfs[i]._id;
      std::map<int, Module >::iterator itr=modules.find(_id);

      if( (intfs[i]._type == 1) && (itr!=modules.end()) )
      {
         modules[_id].GetPlugin()->UnPack(&intfs[i]);
      }
      else if( intfs[i]._type == 2 )
      {
         Geometry* geometry = new Geometry( modules[_id].GetPlugin()->GetID() );
         geometry->SetGeometryDataBuffer( modules[_id].GetPlugin()->GetGeometryDataBuffer() );

         geometry->UnPack(&intfs[i]);

         delete geometry;
      }

   }

   //unpack the Global Param Dialog
   // This is commented out because the computational engine
   // strips the global data out so there is no reason to try
   // to unpack it.
   //globalparam_dlg->UnPack(&intfs[intfs.size()-1]);

   //Now all the data are read from the file. 
   //Let's try to reconstruct the link and the calculate the 
   //first, calculate get the links vector into the modules
   for (i=0; i<links.size(); i++)
   {
      modules[ links[ i ].GetToModule() ].GetLinks()->push_back( links[i] );
      modules[ links[ i ].GetFromModule() ].GetLinks()->push_back( links[i] );
   }

   //Second, calculate the polyes
   for (iter=modules.begin(); iter!=modules.end(); iter++)//=0; i<modules.size(); i++)
   {
      i=iter->first;
      bbox = modules[i].GetPlugin()->GetBBox();
      polynum = modules[i].GetPlugin()->GetNumPoly();
      tmpPoly.resize(polynum);
      modules[i].GetPlugin()->GetPoly(tmpPoly);
      tmpPoly.TransPoly( bbox.x, bbox.y, modules[i].poly); //Make the network recognize its polygon 
   }
  
   for (i=0; i<links.size(); i++)
      links[i].CalcLinkPoly();

   for (i=0; i<tags.size(); i++)
      tags[i].CalcTagPoly();
  
   m_selMod = -1;
   m_selFrPort = -1; 
   m_selToPort = -1; 
   m_selLink = -1; 
   m_selLinkCon = -1; 
   m_selTag = -1; 
   m_selTagCon = -1; 
   xold = yold =0;
  
   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR){ ; }

   Refresh();*/
}

void Network::Save(wxString filename)
{
   //Actually write it to file
   //used to create an nt file
   Package p;
   Pack(p.intfs);

   // Here we wshould loop over all of the following
   //  Newtork
   //  Models
   //  Canvas info
   //  tags
   p.SetPackName("Network");
   p.SetSysId(filename.c_str());

   p.Save();
}

////////////////////////////////////////////////////////
void Network::SaveS( std::string& network_pack )
{
   //Actually write to memory
   //usually used by Frame to submit job to ce
   Package p;
   Pack(p.intfs);

   // Here we wshould loop over all of the following
   //  Newtork
   //  Models
   //  Canvas info
   //  tags
   p.SetPackName("Network");
   p.SetSysId("test.xml");

   bool rv;
   network_pack = p.Save(rv);
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
   }
   modules.clear();

   tags.clear();

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);

   Refresh();
}

////////////////////////////////////////////////////////
void Network::Load(wxString filename)
{
   // Load from the nt file loaded through wx
   wxString tempWx( filename );
   Package p;

   std::string tempString( filename );
   p.SetSysId( tempString.c_str() );
   p.Load();

   intfssize = p.GetIntfsNum();

   UnPack(p.intfs);

   // This function will read the xml file
   // and then create the network and the models
}

//////////////////////////////////////////////////////
void Network::LoadS(const char* inputs)
{
   // Load from memory
   // This is general used by Load Job in Frame
   Package p;
   p.SetSysId("temp.xml");

   if ( std::string( inputs ) != "" )
   {
      p.Load(inputs, strlen(inputs));
      intfssize = p.GetIntfsNum();
      UnPack(p.intfs);
   }
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

void Network::OnGeometry(wxCommandEvent& WXUNUSED(event))
{
   if (m_selMod<0) 
      return;

   std::string m_selmod_name = modules[m_selMod].GetPlugin()->GetName().c_str();
   modules[m_selMod].GetPlugin()->SetIDtoGeometryDataBuffer();
   modules[m_selMod].GetPlugin()->GeometryData();
}
