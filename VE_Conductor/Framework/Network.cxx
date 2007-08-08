/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#include "VE_Conductor/Utilities/CORBAServiceList.h"
#include "VE_Conductor/Framework/Network.h"

#include "VE_Conductor/GUIPlugin/PortDialog.h"
#include "VE_Conductor/GUIPlugin/UserPreferencesDataBuffer.h"
#include "VE_Conductor/GUIPlugin/XMLDataBufferEngine.h"

#include "VE_Conductor/GUIPlugin/UIDialog.h"
#include "VE_Conductor/Utilities/OrbThread.h"
#include "VE_Conductor/Utilities/ParamsDlg.h"
#include "VE_Conductor/DefaultPlugin/DefaultPlugin.h"

#include "VE_Conductor/Framework/Frame.h"
#include "VE_Conductor/Framework/App.h"

#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Model.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/User.h"
#include "VE_Open/XML/StateInfo.h"

#include "VE_Open/XML/CAD/CADAssembly.h"

#include <wx/dc.h>
#include <wx/dcbuffer.h>
#include <wx/utils.h>
#include <wx/progdlg.h>
#include <wx/filename.h>
#include <wx/msgdlg.h>

#include <sstream>
#include <iomanip>
#include <iostream>
#include <cmath>

using namespace VE_Conductor::GUI_Utilities;
using namespace VE_Conductor;

BEGIN_EVENT_TABLE(Network, wxScrolledWindow)
    // see the docs on wxScrolledWindow for more info on this
    // Also see wxPaintEvent
    // overriding this function allows us to handle when things on redrawn
    EVT_PAINT( Network::OnPaint )
    //This is necessary to erase the background
    EVT_ERASE_BACKGROUND( Network::OnEraseBackground )
    //See wxMoveEvent for info on this
    // Motion now only used for dragging
    EVT_MOTION( Network::OnMouseMove )
    //Used for selection
    EVT_LEFT_DOWN( Network::OnMLeftDown )
    EVT_LEFT_UP( Network::OnMLeftUp )
    //brings up the design canvas menu on a specfic module
    EVT_RIGHT_DOWN( Network::OnMRightDown )
    // The following are rightclick menu options
    EVT_MENU( ADD_TAG, Network::OnAddTag )
    EVT_MENU( EDIT_TAG, Network::OnEditTag )
    EVT_MENU( DEL_TAG, Network::OnDelTag )
    EVT_MENU( UIPluginBase::DEL_MOD, Network::OnDelMod )
    EVT_MENU( UIPluginBase::DELETE_PORT, Network::OnDelPort )
    EVT_MENU( Link::DEL_LINK, Network::OnDelLink )
END_EVENT_TABLE()
////////////////////////////////////////////////////////////////////////////////
Network::Network(wxWindow* parent, int id)
  :wxScrolledWindow(parent, id, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL | wxNO_FULL_REPAINT_ON_RESIZE),
tryingLink( false ),
isLoading( false )
{
   modules.clear();
   links.clear();
   userScale.first=1;
   userScale.second=1;
   GetNumUnit()->first=700;
   GetNumUnit()->second=700;
   GetNumPix()->first = 10;
   GetNumPix()->second = 10;
   SetScrollRate( 10, 10 );
   SetVirtualSize( 7000, 7000 );
   //SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
   m_selMod = -1;
   m_selFrPort = -1; 
   m_selToPort = -1; 
   m_selLink = -1; 
   m_selLinkCon = -1; 
   m_selTag = -1; 
   m_selTagCon = -1; 
   xold = yold =0;
   this->parent = parent;
   isDataSet = false;
   frame = dynamic_cast< AppFrame* >( parent->GetParent()->GetParent() );
   dragging = false;
   SetBackgroundColour(*wxWHITE);
   //This is for the paint buffer
   SetBackgroundStyle(wxBG_STYLE_CUSTOM);
}
////////////////////////////////////////////////////////////////////////////////
Network::~Network()
{
    //Pop the link event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
         iter=links.begin(); iter!=links.end(); iter++ )
    {
        RemoveEventHandler( &(*iter) );
    }
    links.clear();
    
    //Pop the tag event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Tag >::iterator 
         iter=tags.begin(); iter!=tags.end(); iter++ )
    {
        RemoveEventHandler( &(*iter) );
    }
    tags.clear();

    //Pop the plugin event handlers to clear these event handlers
    for( std::map< int, Module >::iterator iter = modules.begin(); 
         iter!=modules.end(); iter++)
    {
        RemoveEventHandler( iter->second.GetPlugin() );
    }
    modules.clear();
}
/////////////////////////////////////////////
///////// Event Handlers ////////////////////
/////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
void Network::OnEraseBackground( wxEraseEvent& WXUNUSED( event ) )
{
    //do not implement 
    //this is needed to reduce flicker
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Network::OnPaint(wxPaintEvent& WXUNUSED( event ) )
{
    while ( (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR) ) { ; }
    wxAutoBufferedPaintDC dc(this);
    //DoPrepareDC(dc);
    dc.Clear();
    /*wxColour backgroundColour = GetBackgroundColour();
    dc.SetBrush(wxBrush(backgroundColour));
    dc.SetPen(wxPen(backgroundColour, 1));*/
    
    dc.SetUserScale( userScale.first, userScale.second );
    int xpix, ypix;
    GetScrollPixelsPerUnit( &xpix, &ypix );

    int x, y;
    GetViewStart( &x, &y );
    // account for the horz and vert scrollbar offset
    dc.SetDeviceOrigin( -x * xpix, -y * ypix );
    /*wxRect windowRect(wxPoint(x, y), GetClientSize());    
    
    // We need to shift the client rectangle to take into account
    // scrolling, converting device to logical coordinates    
    CalcUnscrolledPosition(windowRect.x, windowRect.y,
                           & windowRect.x, & windowRect.y);
    dc.DrawRectangle(windowRect);*/

    dc.SetFont( GetFont() );
    ///Now lets draw everything in the current state
    ReDraw(dc); 
    
    while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
}
////////////////////////////////////////////////////////////////////////////////
void Network::OnMLeftDown(wxMouseEvent& event)
{
 	if(event.Dragging())
	{
        return;
	}
    
    wxRect bbox;
    wxPoint pos, temp;
    std::map< int, Module >::iterator iter;
    PORT ports;

    wxClientDC dc(this);
    PrepareDC(dc);
    dc.SetUserScale( userScale.first, userScale.second );
    
    wxPoint evtpos = event.GetLogicalPosition(dc);

    long x = evtpos.x;
    long y = evtpos.y;

    //Clear selections
    //if (m_selMod >= 0)
     UnSelectMod(dc);
    //if (m_selLink >= 0)
	 UnSelectLink(dc);
    if (m_selTag >= 0)
        UnSelectTag(dc);

    //Refresh(true);
    //Update();

    //Select Mod/Link/Tag
    SelectMod(x, y, dc);
    if(m_selMod >= 0)
    {
        modules[m_selMod].GetPlugin()->SetHighlightFlag( true );
        //Select the ports for a plugin
        bbox = modules[m_selMod].GetPlugin()->GetBBox();
        temp.x = x - bbox.x;
        temp.y = y - bbox.y;

        relative_pt = temp;
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
    else
    {
        SelectLink(x, y );
        if (m_selLink>=0)
        {
            links[m_selLink].SetHighlightFlag( true );
            for (unsigned int i=0; i<links[m_selLink].GetPoints()->size(); i++)
            if (computenorm( evtpos, *(links[m_selLink].GetPoint( i )) )<=3)
            {
                m_selLinkCon=i;
                break;
            }
        }
        else
            SelectTag(x, y); 
    }
    Refresh(true);
    //Update();
}
////////////////////////////////////////////////////////////////////////////////
void Network::OnMouseMove(wxMouseEvent& event)
{
/*  wxClientDC dc(this);
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
	   // To avoid the shortcut evaluation
	   SelectMod(x, y, dc);
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

      //if (m_selMod < 0) //SelectMod(x, y)<0 ) 
	  //	{	
      //   UnSelectMod(dc); //Unselect only get called by state changed from something selected state to nothing selected state
      //}
	  
	  if (m_selLink>=0 && SelectLink(x, y )<0)
	   {   
         UnSelectLink(dc);
      }
      
	  if (m_selTag>=0 && SelectTag(x, y)<0)
	   {   
         UnSelectTag(dc);
      }
   }
   else //dragging
   {*/
	if (event.Dragging())
	{	
		dragging = true;
		//drag link connector
		if (m_selLinkCon >= 0 && m_selLink >= 0)
		{		
            //std::cout << " drag link connector " << std::endl;
            links[m_selLink].SetHighlightFlag( true );
			wxClientDC dc(this);
			DoPrepareDC(dc);
			dc.SetUserScale( userScale.first, userScale.second );
			wxPoint evtpos = event.GetLogicalPosition(dc);
			long x = evtpos.x;
			long y = evtpos.y;
			MoveLinkCon(x, y, m_selLink, m_selLinkCon, dc);
		}
		
		//drag tag
		else if (m_selTag>=0 && m_selTagCon<0)
		{		
            //std::cout << " drag tag  " << std::endl;
			wxClientDC dc(this);
			DoPrepareDC(dc);
			dc.SetUserScale( userScale.first, userScale.second );
			wxPoint evtpos = event.GetLogicalPosition(dc);
			long x = evtpos.x;
			long y = evtpos.y;
			MoveTag(x, y, m_selTag, dc);
		}

		//drag tag connector
		else if (m_selTag>=0 && m_selTagCon>=0)
		{		
            //std::cout << " drag tag connector " << std::endl;
			wxClientDC dc(this);
			DoPrepareDC(dc);
			dc.SetUserScale( userScale.first, userScale.second );
			wxPoint evtpos = event.GetLogicalPosition(dc);
			long x = evtpos.x;
			long y = evtpos.y;
			MoveTagCon(x, y, m_selTag, m_selTagCon, dc);
		}

		//drag input port
		else if (m_selMod>=0 && m_selFrPort>=0)
		{		
            modules[m_selMod].GetPlugin()->SetHighlightFlag( true );
            //std::cout << " drag link input " << std::endl;
			wxClientDC dc(this);
			dc.SetUserScale( userScale.first, userScale.second );
			DoPrepareDC(dc);
			wxPoint evtpos = event.GetLogicalPosition(dc);
			long x = evtpos.x;
			long y = evtpos.y;
            tryingLink = true;
           //std::cout << x << " " << y << " " <<  evtpos.x << " " <<  evtpos.y << std::endl;
			TryLink(x, y, m_selMod, m_selFrPort, dc, true); // draw input ports
		}

		//drag output port
		else if (m_selMod>=0 && m_selToPort>=0)
		{		
            modules[m_selMod].GetPlugin()->SetHighlightFlag( true );
            //std::cout << " drag link output " << std::endl;
			wxClientDC dc(this);
			dc.SetUserScale( userScale.first, userScale.second );
			DoPrepareDC(dc);
			wxPoint evtpos = event.GetLogicalPosition(dc);
			long x = evtpos.x;
			long y = evtpos.y;
            tryingLink = true;
			long xM = event.GetPosition().x;
			long yM = event.GetPosition().y;
            //std::cout << "dlo scale " << userScale.first << " " << userScale.second << std::endl;
            //std::cout << "dlo pos " << xM << " " << yM << " " <<  evtpos.x << " " <<  evtpos.y << std::endl;
            //std::cout << "dlo mod " <<m_selMod << " " <<  m_selToPort << std::endl;
			TryLink(x, y, m_selMod, m_selToPort, dc, false); // draw output ports
            //std::cout << "end dlo " << std::endl;
		}

		//drag module
		else if (m_selMod >= 0 && m_selFrPort < 0 && m_selToPort < 0)
		{		
            modules[m_selMod].GetPlugin()->SetHighlightFlag( true );
            //std::cout << " drag module " << std::endl;
			wxClientDC dc(this);
			DoPrepareDC(dc);
			dc.SetUserScale( userScale.first, userScale.second );
			wxPoint evtpos = event.GetLogicalPosition(dc);
			long x = evtpos.x;
			long y = evtpos.y;
			MoveModule(x, y, m_selMod);
		}
	}
}

/////////////////////////////////////////////////////////////////////
void Network::OnMLeftUp(wxMouseEvent& event)
{
	//no longer dragging
	dragging = false;

	//release link connector
	if (m_selLinkCon>=0 && m_selLink>=0)
	{
		wxClientDC dc(this);
		PrepareDC(dc);
		dc.SetUserScale( userScale.first, userScale.second );

		wxPoint evtpos = event.GetLogicalPosition(dc);
		long x = evtpos.x;
		long y = evtpos.y;

		// We will create the link connector (basically a bend point)
		DropLinkCon(x, y, m_selLink, m_selLinkCon, dc);
		m_selLinkCon = -1;
		//m_selLink=-1;
		//Refresh(true);
		//Update();
		//links[m_selLink].DrawLinkCon( true, userScale ); 
	}

	//release tag
	else if (m_selTag>=0 && m_selTagCon<0)
	{
		wxClientDC dc(this);
		PrepareDC(dc);
		dc.SetUserScale( userScale.first, userScale.second );

		wxPoint evtpos = event.GetLogicalPosition(dc);
		long x = evtpos.x;
		long y = evtpos.y;

		// drop the tag we just created
		DropTag(x, y, m_selTag, dc);
		//m_selTag=-1;
	}

	//release tag connection
	else if (m_selTag>=0 && m_selTagCon>=0)
	{
		wxClientDC dc(this);
		PrepareDC(dc);
		dc.SetUserScale( userScale.first, userScale.second );

		wxPoint evtpos = event.GetLogicalPosition(dc);
		long x = evtpos.x;
		long y = evtpos.y;

		// We will create the tag connector (basically a bend point)
		DropTagCon(x, y, m_selTag, m_selTagCon, dc);
		//m_selTag=-1;
		m_selTagCon=-1;
	}

	//release start point of link
	else if (m_selMod>=0 && m_selFrPort>=0)
	{
		wxClientDC dc(this);
		PrepareDC(dc);
		dc.SetUserScale( userScale.first, userScale.second );

		wxPoint evtpos = event.GetLogicalPosition(dc);
		long x = evtpos.x;
		long y = evtpos.y;

		// drop the start point of the link
		DropLink(x, y, m_selMod, m_selFrPort, dc, true);
		//m_selMod = -1;
		m_selFrPort = -1;
	}

	//release end point of link
	else if (m_selMod>=0 && m_selToPort>=0)
	{
		wxClientDC dc(this);
		PrepareDC(dc);
		dc.SetUserScale( userScale.first, userScale.second );

		wxPoint evtpos = event.GetLogicalPosition(dc);
		long x = evtpos.x;
		long y = evtpos.y;

		// drop the final point of the link
		DropLink(x, y, m_selMod, m_selToPort, dc, false);
		//m_selMod = -1;
		m_selToPort = -1;
	}

	//release the module
	else if (m_selMod>=0 && m_selFrPort<0 && m_selToPort<0)
	{
		wxClientDC dc(this);
		PrepareDC(dc);
		dc.SetUserScale( userScale.first, userScale.second );

		wxPoint evtpos = event.GetLogicalPosition(dc);
		long x = evtpos.x;
		long y = evtpos.y;

		//drop a module after dragging it around
		DropModule(x, y, m_selMod );
	}
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
void Network::OnMRightDown(wxMouseEvent& event)
{
   wxClientDC dc(this);
   PrepareDC(dc);
   dc.SetUserScale( userScale.first, userScale.second );
   
   /////////////////////////////////////////////////
   wxPoint evtpos = event.GetLogicalPosition( dc );
   
   //Clear selections
   //if (m_selMod >= 0)
	//   UnSelectMod(dc);
   //if (m_selLink >= 0)
	//   UnSelectLink(dc);
   
   //Select Mod/Link
   /*SelectMod(x, y, dc);
   if (m_selMod < 0)
	   SelectLink(x, y );*/
    SelectTag(  evtpos.x,  evtpos.y );
    Refresh(true);
   //Update();
   /////////////////////////////////////////////////

   wxMenu the_pop_menu( _("Action"));
   the_pop_menu.Append(ADD_TAG, _("Add Tag")); //This will always be enable
   the_pop_menu.Append(EDIT_TAG, _("Edit Tag") );
   the_pop_menu.Append(DEL_TAG, _("Delete Tag") );

   if( m_selTag>=0 )
   {
      the_pop_menu.Enable(EDIT_TAG, true);
      the_pop_menu.Enable(DEL_TAG, true);
   }
   else
   {
      the_pop_menu.Enable(EDIT_TAG, false);
      the_pop_menu.Enable(DEL_TAG, false);
   }

   action_point = event.GetLogicalPosition(dc);
   PopupMenu(&the_pop_menu, event.GetPosition());


   m_selMod = -1;
   m_selFrPort = -1; 
   m_selToPort = -1; 
   m_selLink = -1; 
   m_selLinkCon = -1; 
   m_selTag = -1; 
   m_selTagCon = -1; 
   xold = yold =0;
}
////////////////////////////////////////////////////////////////////////////////
//////// Menu event handlers ////////////////////////
////////////////////////////////////////////////////////////////////////////////
void Network::OnAddTag(wxCommandEvent& WXUNUSED(event))
{
   wxTextEntryDialog dialog(this,_("Tag Editor"), _("Please enter the text for the tag : "),_("this is a tag"), wxOK);

   if (dialog.ShowModal() == wxID_OK)
      AddTag(action_point.x, action_point.y, dialog.GetValue());
}
////////////////////////////////////////////////////////////////////////////////
void Network::OnEditTag(wxCommandEvent& WXUNUSED(event))
{
   wxClientDC dc(this);
   PrepareDC(dc);

   dc.SetUserScale( userScale.first, userScale.second );

   if(m_selTag >= 0)
   {
	   wxString tag_text = *( tags[ m_selTag ].GetTagText() );
	   wxTextEntryDialog dialog(this,_("Tag Editor"), _("Please enter the text for the tag : "),tag_text, wxOK);

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
	   m_selTag = -1;
	   Refresh(true);
   }
}

/////////////////////////////////////////////////////
void Network::OnDelTag(wxCommandEvent& WXUNUSED(event))
{
	if( m_selTag < 0 )
	{
        return;
	}
    
    int answer = wxMessageBox( _("Do you really want to delete this tag?"), _("Confirmation"), wxYES_NO);
    if( answer != wxYES )
    {
        return;
    }

    while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);

    //Pop the tag event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Tag >::iterator 
         iter=tags.begin(); iter!=tags.end(); iter++ )
    {
        RemoveEventHandler( &(*iter) );
    }

    int i;
    std::vector< Tag >::iterator iter;
    for( iter = tags.begin(), i=0; iter != tags.end(); ++i )
    {  
        if( i == m_selTag )
        {
            iter = tags.erase( iter );
            m_selTag=-1;
            break;
        }
        else
        {
            ++iter;
        }
    }
    
    //Pop the tag event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Tag >::iterator 
       iter=tags.begin(); iter!=tags.end(); iter++ )
    {
        PushEventHandler( &(*iter) );
    }
    
    while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);

    Refresh(true);
}
////////////////////////////////////////////////////////////////////////////////
void Network::OnDelLink(wxCommandEvent& event )
{
    while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
    //Pop the link event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
         iter=links.begin(); iter!=links.end(); iter++ )
    {
        RemoveEventHandler( &(*iter) );
    }

    std::string* selLink = static_cast< std::string* >( event.GetClientData() );
    for( std::vector< Link >::iterator iter = links.begin(); 
        iter!=links.end(); )
    {
        if( iter->GetUUID() == *selLink )
        {
            iter = links.erase( iter );
            break;
        }
        else
        {
            ++iter;
        }
    }

    //Pop the link event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
         iter=links.begin(); iter!=links.end(); iter++ )
    {
        PushEventHandler( &(*iter) );
    }

    while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);

    Refresh(true);
    //Update();
}
////////////////////////////////////////////////////////////////////////////////
void Network::OnDelMod(wxCommandEvent& event )
{
    while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR){ ; }

    //Pop the link event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
         iter=links.begin(); iter!=links.end(); iter++ )
    {
        RemoveEventHandler( &(*iter) );
    }

    // Need to delete all links associated with this particular module
    // first, delete all the links connects to it
    int* selMod = static_cast< int* >( event.GetClientData() );
    std::vector< Link >::iterator iter3;
    for ( iter3=links.begin(); iter3!=links.end(); )
    {
        if( (iter3->GetFromModule() == *selMod) || 
            (iter3->GetToModule() == *selMod) 
            )
        {
            iter3 = links.erase( iter3 );
        }
        else
        {
            ++iter3;
        }
    }

    //Pop the link event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
    iter=links.begin(); iter!=links.end(); iter++ )
    {
        PushEventHandler( &(*iter) );
    }
    ///Need to clear out the vector of polygon boxes as well
    ///XXX
    /////////////

    //Now delete the plugin from the module and then remove from the map
    std::map< int, Module >::iterator iter;
    iter = modules.find( *selMod );
    if( iter != modules.end() )
    {
        //delete modules[m_selMod].GetPlugin();
        modules.erase( iter );
        m_selLink = -1;
        m_selMod = -1;
    }

    while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR){ ; }

    Refresh( true );
}
////////////////////////////////////////////////////////////////////////////////
void Network::OnDelPort(wxCommandEvent& event )
{
    while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR){ ; }
    
    //Pop the link event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
        iter=links.begin(); iter!=links.end(); iter++ )
    {
        RemoveEventHandler( &(*iter) );
    }
    
    // Need to delete all links associated with this particular module
    // first, delete all the links connects to it
    VE_XML::VE_Model::Port* selPort = 
        static_cast< VE_XML::VE_Model::Port* >( event.GetClientData() );
    for( std::vector< Link >::iterator iter3=links.begin(); 
        iter3!=links.end(); )
    {
        if( (iter3->GetFromModule() == m_selMod ) && 
            (iter3->GetFromPort() == selPort->GetPortNumber() ) )
        {
            iter3 = links.erase( iter3 );
        }
        else if( (iter3->GetToModule() == m_selMod ) && 
            (iter3->GetToPort() ==  selPort->GetPortNumber() ) )
        {
            iter3 = links.erase( iter3 );
        }
        else
        {
            ++iter3;
        }
    }
    
    //Pop the link event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
        iter=links.begin(); iter!=links.end(); iter++ )
    {
        PushEventHandler( &(*iter) );
    }
    
    while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR){ ; }
}
////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////
///// Selection Functions ///////////
/////////////////////////////////////
int Network::SelectMod( int x, int y, wxDC &dc )
{
    // This function checks to see which module your mouse is over based
    // on the x and y location of your mouse on the design canvas
    wxPoint temp;
    temp.x = x;
    temp.y = y;
    for( std::map< int, Module >::iterator iter = modules.begin(); 
        iter!=modules.end(); iter++)
    {
        if( iter->second.GetPolygon()->inside( temp ) )
        {
            // now we are officially selected
            m_selMod = iter->first;
            return m_selMod;
        }
    }
    m_selMod = -1;
    return m_selMod;
}
////////////////////////////////////////////////////////////////////////////////
void Network::UnSelectMod( wxDC& dc )
{
	for( std::map<int, Module>::iterator iter = modules.begin(); 
        iter != modules.end(); ++iter )
    {
		iter->second.GetPlugin()->SetHighlightFlag( false );
    }
	m_selMod = -1;
}
////////////////////////////////////////////////////////////////////////////////
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
	      //links[i].DrawLinkCon( true, userScale ); 
	      m_selLink = i;
	      return i;
	   }
   }
   m_selLink = -1;
   return -1;
}
////////////////////////////////////////////////////////////////////////////////
void Network::UnSelectLink(wxDC &dc)
{
   for ( size_t i = 0; i < links.size(); ++i )
      links[i].SetHighlightFlag( false );
   m_selLink = -1;
}
////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////
void Network::UnSelectTag(wxDC &dc)
{
  tags[m_selTag].DrawTagCon( false, userScale );
  m_selTag = -1;
}
////////////////////////////////////////////////////////////////////////////////
/////////////// Misc Functions //////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
void Network::CleanRect(wxRect box, wxDC &dc)
{
    wxRect windowRect( wxPoint(0,0), GetClientSize() );
    CalcUnscrolledPosition(windowRect.x, windowRect.y, 
        &windowRect.x, &windowRect.y);
    dc.DrawRectangle( windowRect );
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
void Network::MoveModule(int x, int y, int mod)
{
  if (mod < 0) // no module is selected
    return;   
  
  UIPluginBase *cur_module;
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
  ypos = (int)( 1.0 * ypos / userScale.second );
  oldxpos = xpos;
  oldypos = ypos;

  sx = (int)( 1.0*sx / userScale.first );
  sy = (int)( 1.0*sy / userScale.second );
  w = (int)( 1.0*w / userScale.first );
  h = (int)( 1.0*h / userScale.second ); 
  
  ex=xpos*xunit+w;
  ey=ypos*yunit+h;
  
  cur_module = modules[mod].GetPlugin();
      
  //bbox = cur_module->GetBBox(); //Get the Boundoing box of the module
  
  if ( x > ex-bbox.width )
    xpos += 1;//userScale.first;
  else if ( x < (ex-w)+relative_pt.x )
    xpos -= 1;//userScale.first;
  
  if ( y > ey-bbox.height )
    ypos += 1;//userScale.second;
  if ( y < (ey-h+relative_pt.y) )
    ypos -= 1;//userScale.second;
  
   if ( x-relative_pt.x+bbox.width > sx )
   {
      GetNumUnit()->first += 2;
      SetScrollbars( GetNumPix()->first, GetNumPix()->second, GetNumUnit()->first, GetNumUnit()->second );
      scroll = true;
   }

   if ( y-relative_pt.y+bbox.height > sy )
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
         //links.at( i ).DrawLink( false, userScale );
         //std::cout << " move module output " << std::endl;
         wxPoint pos = GetPointForSelectedPlugin( mod, links.at( i ).GetFromPort(), "output" );
         *(links.at( i ).GetPoint( 0 )) = pos;
      }
      //if the modules are the same
      if ( (links.at( i ).GetToModule() == mod) )
      {
         //links.at( i ).DrawLink( false, userScale );
          //std::cout << " move module input " << std::endl;
         wxPoint pos = GetPointForSelectedPlugin( mod, links.at( i ).GetToPort(), "input" );
         *(links.at( i ).GetPoint( links.at( i ).GetPoints()->size()-1 )) = pos;
      }
   }
    
   if (oldxpos!=xpos||oldypos!=ypos||scroll)
   {
      xpos = (int)( 1.0 * xpos * userScale.first );
      ypos = (int)( 1.0 * ypos * userScale.second );
      Scroll(xpos, ypos);
   }

   Refresh(true);
   //Update();
}
/////////////////////////////////////////////////////////////
void Network::DropModule(int ix, int iy, int mod )
{
  wxRect bbox; //Bounding box  
  int sx, sy;
  double r;
  int vx, vy;
  int x, y;
  UIPluginBase * cur_module;
  bool scroll = false; 

  //In drag mode
  if (mod < 0) // no module is selected
    return; 
  
  GetVirtualSize(&sx, &sy);
  sx = (int)( 1.0*sx / userScale.first );
  sy = (int)( 1.0*sy / userScale.second );
  
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
  //  vy = vy / userScale.second;
  
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
}
/////////////////////////////////////////////////////////////////////////
void Network::TryLink(int x, int y, int mod, int pt, wxDC& dc, bool flag)
{
   PORT ports;
   wxRect bbox;
   int dest_mod=-1;
   //This loop causes a tremendous perfomance decrease
   wxPoint temp;
   //must remove the scale because the points are scaled in the draw function
    x = x / userScale.first;//dc.DeviceToLogicalX( x );
    y = y / userScale.second;//dc.DeviceToLogicalY( y );
    temp.x = x;//dc.LogicalToDeviceX( x );
    temp.y = y;//dc.LogicalToDeviceY( y );
   for( std::map< int, Module >::iterator iter = modules.begin(); iter!=modules.end(); iter++)
   {
      if ( iter->second.GetPolygon()->inside( temp ) && dest_mod!=mod)
      {
         dest_mod = iter->first;
          modules[dest_mod].GetPlugin()->SetHighlightFlag( true );
         break;
      }
   }
   //std::cout << " dest_Mod " << dest_mod << std::endl;
   //iter=modules.find(dest_mod);	

   //if (t!=dest_mod && iter!=modules.end())
   //   DrawPorts(modules[dest_mod].GetPlugin(), false); //wipe the ports

   //dest_mod = t;

   //DrawPorts(modules[mod].GetPlugin(), false); //wipe the ports

   wxPoint offSet;
    //DrawPorti(modules[mod].GetPlugin(), pt, flag);
   if( flag )
   {
       int num = modules[mod].GetPlugin()->GetNumIports();
       ports.resize(num);
       modules[mod].GetPlugin()->GetIPorts(ports);
       offSet = GetPointForSelectedPlugin( mod, ports[ pt ].GetPortNumber(), "input" );
   }
   else
   {
       int num = modules[mod].GetPlugin()->GetNumOports();
       ports.resize(num);
       modules[mod].GetPlugin()->GetOPorts(ports);
       /*for(size_t i = 0; i < ports.size(); ++i )
       {
           std::cout << " output port " <<  i << " " << ports[ i ].GetPortNumber() << std::endl;
       }*/
       offSet = GetPointForSelectedPlugin( mod, ports[ pt ].GetPortNumber(), "output" );
   }
   //std::cout << ConvertUnicode( modules[mod].GetPlugin()->GetName().c_str() ) << std::endl;
   //dc.SetPen(*wxWHITE_PEN);
   //dc.DrawLine( offSet.x, offSet.y, xold, yold);

   //dc.SetPen(*wxBLACK_PEN);
   //std::cout << " before " << offSet.x << " " << offSet.y << " " << x << " " << y << std::endl;
   //offSet.x = dc.LogicalToDeviceX( offSet.x );
   //offSet.y = dc.LogicalToDeviceY( offSet.y );
   //std::cout << " after " << offSet.x << " " << offSet.y << " " << x << " " << y << std::endl;
   //dc.DrawLine( offSet.x, offSet.y, x, y);
   point1 = offSet;
   point2 = wxPoint( x, y );
   tryingLink = true;
   Refresh(true);
   //Update();

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
   std::map< int, Module >::iterator iter;

   dest_mod = dest_port = -1;
   temp.x = x;
   temp.y = y;
   
    for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
        if ( modules[ iter->first ].GetPolygon()->inside( temp ) )
        {
            dest_mod = iter->first;
            bbox = modules[dest_mod].GetPlugin()->GetBBox();

            temp.x = x - bbox.x;
            temp.y = y - bbox.y;
            break;
        }
    }

   // If input port
   wxPoint offSet;
   int acutallPortNumber = -1;
   int acutallDestPortNumber = -1;
   if ( flag )
   {
       int num = modules[ mod ].GetPlugin()->GetNumIports();	
       ports.resize(num);
       modules[ mod ].GetPlugin()->GetIPorts( ports );
       offSet = GetPointForSelectedPlugin( mod, ports[ pt ].GetPortNumber(), "input" );
       acutallPortNumber = ports[ pt ].GetPortNumber();
      if (dest_mod>=0)
      {
         ports.resize( modules[dest_mod].GetPlugin()->GetNumOports() );
         modules[dest_mod].GetPlugin()->GetOPorts(ports);
   
         for( size_t i=0; i < ports.size(); i++)
         {
            wxPoint tempPoint( ports[i].GetPortLocation()->GetPoint().first, ports[i].GetPortLocation()->GetPoint().second );
            if (computenorm(temp, tempPoint)<=10) 
            {
               //Also, we need to check if port Type is the same
				if (IsPortCompatible(dest_mod, i, mod, pt))
				//if (IsPortCompatible(mod, pt, dest_mod, i))
				{
					dest_port = i;
                    acutallDestPortNumber = ports[ dest_port ].GetPortNumber();
					break;
				}
            }
         }
      }
   }
   else    // If ouput port
   {
       int num = modules[ mod ].GetPlugin()->GetNumOports();	
       ports.resize(num);
       modules[ mod ].GetPlugin()->GetOPorts( ports );
       offSet = GetPointForSelectedPlugin( mod, ports[ pt ].GetPortNumber(), "output" );
       acutallPortNumber = ports[ pt ].GetPortNumber();

      // check if the drop point is a out port
      if (dest_mod>=0)
      {
         ports.resize( modules[dest_mod].GetPlugin()->GetNumIports() );
         modules[dest_mod].GetPlugin()->GetIPorts(ports);
         for ( size_t i=0; i< ports.size(); i++)
         {
            wxPoint tempPoint( ports[i].GetPortLocation()->GetPoint().first, ports[i].GetPortLocation()->GetPoint().second );
            if (computenorm(temp, tempPoint)<=10)
            {
				//if (IsPortCompatible(dest_mod, i, mod, pt))
				if (IsPortCompatible(mod, pt, dest_mod, i))
				{
					dest_port = i;
                    acutallDestPortNumber = ports[ dest_port ].GetPortNumber();
					break;
				}
            }
         }
      }
   }  

   //Wipe off the test line
   //dc.SetPen(*wxWHITE_PEN);
   //dc.DrawLine( offSet.x, offSet.y, xold, yold);

   // if it is a good link
   // and a user can not link to itself
   //std::cout << dest_mod << " " << acutallDestPortNumber << " " << mod << " " << acutallPortNumber << std::endl;
   if (dest_mod>=0 && acutallDestPortNumber>=0 && ( (dest_mod!=mod) || (acutallDestPortNumber!=acutallPortNumber) ) )
   {
        Link ln( this );
        if ( flag ) // if input port
        {
            ln.SetToModule( mod );
            ln.SetToPort( acutallPortNumber );
            ln.SetFromModule( dest_mod );
            ln.SetFromPort( acutallDestPortNumber );
        }
        else // if output port
        {
            ln.SetToModule( dest_mod );
            ln.SetToPort( acutallDestPortNumber );
            ln.SetFromModule( mod );
            ln.SetFromPort( acutallPortNumber );
        }
   
        // check for duplicate links
        bool found = false;
        for ( size_t i=0; i< links.size(); i++)
        {
            if ( links.at( i ) == ln )
            {
                found = true;
            }
        }

        if ( !found ) // no duplicate links are allowed
        {
            //Pop the link event handlers to clear these event handlers
            for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
                iter=links.begin(); iter!=links.end(); iter++ )
            {
                RemoveEventHandler( &(*iter) );
            }

            wxPoint pos;
            // Get first port point for the link
            pos = GetPointForSelectedPlugin( ln.GetFromModule(), ln.GetFromPort(), "output" );
            ln.SetPoint( &pos );//->push_back( GetPointForSelectedPlugin( ln.GetFromModule(), ln.GetFromPort(), "output" ) );

            // Get last port point for the link
            pos = GetPointForSelectedPlugin( ln.GetToModule(), ln.GetToPort(), "input" );
            ln.SetPoint( &pos );//->push_back( GetPointForSelectedPlugin( ln.GetToModule(), ln.GetToPort(), "input" ) );
            ln.CalcLinkPoly();
            links.push_back( ln );
            links.back().SetDCScale( &userScale );
            VE_XML::VE_Model::Link object;
            links.back().SetUUID( object.GetID() );

            for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
                iter=links.begin(); iter!=links.end(); iter++ )
            {
                PushEventHandler( &(*iter) );
            }
        }
    }

   Refresh( true );
   //m_selMod = -1;
   m_selFrPort = -1;
   m_selToPort = -1;
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
    sy = (int)( 1.0*sy / userScale.second );
    w = (int)( 1.0*w / userScale.first );
    h = (int)( 1.0*h / userScale.second ); 
    
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
    
    *(links[ln].GetPoint( ln_con )) = wxPoint(x,y);
    
    if ( oldxpos!=xpos || oldypos!=ypos || scroll)
    {
        xpos = (int)( 1.0 * xpos * userScale.first );
        ypos = (int)( 1.0 * ypos * userScale.second );
        
        Scroll(xpos, ypos);
    }
    
    Refresh(true);
    //Update();
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
    sy = (int)( 1.0*sy / userScale.second );
    //  w = w / userScale.first;
    //h = h / userScale.second; 
    
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
  sy = (int)( 1.0*sy / userScale.second );
  w = (int)( 1.0*w / userScale.first );
  h = (int)( 1.0*h / userScale.second ); 

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
  //tags[t].DrawTag( false, userScale );
  //tags[t].DrawTagCon( false, userScale );
  *(tags[t].GetConnectorsPoint( t_con ))=wxPoint(x,y);
  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = (int)( 1.0 * xpos * userScale.first );
      ypos = (int)( 1.0 * ypos * userScale.second );
      
      Scroll(xpos, ypos);
    }

  tags[t].DrawTagCon( true, userScale );
  Refresh(true);
  //Update();
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
  sy = (int)( 1.0*sy / userScale.second );
  //  w = w / userScale.first;
  //  h = h / userScale.second; 
     
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

  Refresh(true);
  //Update();
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
  sy = (int)( 1.0*sy / userScale.second );
  w = (int)( 1.0*w / userScale.first );
  h = (int)( 1.0*h / userScale.second ); 

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
  //tags[t].DrawTag( false, userScale );
  //tags[t].DrawTagCon( false, userScale );

  tags[t].GetBoundingBox()->x = x;//-tag_rpt.x;
  tags[t].GetBoundingBox()->y = y;//-tag_rpt.y;

  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = (int)( 1.0 * xpos * userScale.first );
      ypos = (int)( 1.0 * ypos * userScale.second );
      
      Scroll(xpos, ypos);
    }

  tags[t].DrawTagCon( true, userScale );
  Refresh(true);
  //Update();
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
  sy = (int)(1.0*sy / userScale.second);
  //  w = w / userScale.first;
  //  h = h / userScale.second; 
      
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
  
  tags[t].GetBoundingBox()->x = x;// - tag_rpt.x;
  tags[t].GetBoundingBox()->y = y;// - tag_rpt.y;

  tags[t].CalcTagPoly();

  Refresh(true);
  //Update();
}
//////////////////////////////////////////////////////
///////// Add to Network Funtions ////////////////////
//////////////////////////////////////////////////////
void Network::AddTag(int x, int y, wxString text)
{
    while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
    //Pop the tag event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Tag >::iterator 
         iter=tags.begin(); iter!=tags.end(); iter++ )
    {
        RemoveEventHandler( &(*iter) );
    }
    
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

   //Pop the tag event handlers to clear these event handlers
   for( std::vector< VE_Conductor::GUI_Utilities::Tag >::iterator 
        iter=tags.begin(); iter!=tags.end(); iter++ )
   {
       PushEventHandler( &(*iter) );
   }

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
   
   Refresh(true);
}

//////////////////////////////////////////////////////////////
void Network::AddtoNetwork(UIPluginBase *cur_module, std::string cls_name)
{
    while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
    POLY tmpPoly;
    int num;

    int id;
    std::map<int, Module>::iterator mit;
    while (1)
    {
        id = wxNewId();
        if ( id > 9999 )
        id=id%9999;

        mit = modules.find(id);
        if( mit == modules.end() )
            break;
    };
    //do it this way because we don't have equal operators setup for plugins
    Module mod;
    modules[id]=mod;

    wxRect bbox; //Bounding box  

    bbox = cur_module->GetBBox(); //Get the Boundoing box of the modul 

    cur_module->SetPos( GetFreePos(bbox) ); //Set the new modules position to be a free space allocated by the network according to its bounding box
    bbox = cur_module->GetBBox();
    modules[id].SetPlugin( cur_module );

    num = cur_module->GetNumPoly();
    tmpPoly.resize(num);
    cur_module->GetPoly(tmpPoly); 
    VE_Conductor::GUI_Utilities::Polygon newPolygon;
    *(newPolygon.GetPolygon()) = tmpPoly;

    newPolygon.TransPoly( bbox.x, bbox.y, *(modules[id].GetPolygon()) ); //Make the network recognize its polygon 
    modules[id].SetClassName( cls_name );

    modules[id].GetPlugin()->SetID(id);
    modules[id].GetPlugin()->SetCORBAService( VE_Conductor::CORBAServiceList::instance() );
    modules[id].GetPlugin()->SetDialogSize( frame->GetAppropriateSubDialogSize() );
    //modules.push_back(mod);
    sbboxes.push_back(bbox);
    //  for (i=0; i<modules.size(); i++)
    //Setup the event handlers for the plugin
    PushEventHandler( modules[id].GetPlugin() );
    Refresh(true);
    //Update();
    while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
}
////////////////////////////////////////
/////// Draw Functions /////////////////
////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
void Network::ReDraw(wxDC &dc)
{
    // this function redraws the design canvas
    dc.SetPen(*wxBLACK_PEN);
    dc.SetBrush(*wxWHITE_BRUSH);
    dc.SetBackground(*wxWHITE_BRUSH);
    //dc.SetBackgroundMode(wxSOLID);

    // redraw all the active plugins
    for( std::map<int, Module>::iterator iter = modules.begin(); 
        iter != modules.end(); iter++)
    {
        iter->second.GetPlugin()->DrawPlugin( &dc );
        ///Set everything back to false for next loop
        if( iter->first != m_selMod )
		{
            iter->second.GetPlugin()->SetHighlightFlag( false );
        }
    }

    // draw all the links
    for ( size_t i = 0; i < links.size(); ++i )
    {
        links.at( i ).DrawLink( &dc );
        ///Set everything back to false for next loop
        if( i != m_selLink )
        {
            links.at( i ).SetHighlightFlag( false );
        }
    }

    // draw all the tags
    for( size_t i = 0; i < tags.size(); ++i )
    {
        tags[i].DrawTag( true, dc, userScale );
    }

    if( IsDragging() )
    {
        if( tryingLink )
        {
            dc.DrawLine( point1.x, point1.y, point2.x, point2.y );
            tryingLink = false;
        }
    }
}
/////////////////////////////////////////////////////////
////// Math Functions for the points and polygons ///////
/////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
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
    //Newtork
    veNetwork = VE_XML::VE_Model::Network();
    //Need to delete network first
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( &veNetwork, "veNetwork" ) );

    veNetwork.GetDataValuePair( -1 )->SetData( "m_xUserScale", userScale.first );
    veNetwork.GetDataValuePair( -1 )->SetData( "m_yUserScale", userScale.second );
    veNetwork.GetDataValuePair( -1 )->SetData( "nPixX", static_cast< long int >( numPix.first ) );
    veNetwork.GetDataValuePair( -1 )->SetData( "nPixY", static_cast< long int >( numPix.second ) );
    veNetwork.GetDataValuePair( -1 )->SetData( "nUnitX", static_cast< long int >( numUnit.first ) );
    veNetwork.GetDataValuePair( -1 )->SetData( "nUnitY", static_cast< long int >( numUnit.second ) );

    //Update the links
    for( size_t i = 0; i < links.size(); ++i )
    {
        VE_XML::VE_Model::Link* xmlLink = veNetwork.GetLink( -1 );
        xmlLink->GetFromModule()->SetData( modules[ links[i].GetFromModule() ].GetClassName(), static_cast< long int >( links[i].GetFromModule() ) );
        xmlLink->GetToModule()->SetData( modules[ links[i].GetToModule() ].GetClassName(), static_cast< long int >( links[i].GetToModule() ) );
        *(xmlLink->GetFromPort()) = static_cast< long int >( links[i].GetFromPort() );
        *(xmlLink->GetToPort()) = static_cast< long int >( links[i].GetToPort() );
        xmlLink->SetLinkName( ConvertUnicode( links.at( i ).GetName().c_str() ) );
        xmlLink->SetID( links.at( i ).GetUUID() );

        //Try to store link cons,
        //link cons are (x,y) wxpoint
        //here I store x in one vector and y in the other
        for ( size_t j = 0; j < links[ i ].GetNumberOfPoints(); ++j )
        {
            xmlLink->GetLinkPoint( j )->SetPoint( 
                std::pair< unsigned int, unsigned int >( 
                links[ i ].GetPoint( j )->x, links[ i ].GetPoint( j )->y ) );
        }
    }

    //Update the tags
    for( size_t i = 0; i < tags.size(); ++i )
    {
        veNetwork.AddTag( tags.at( i ).GetVETagPtr() );
    }
       
    //Models
    for( std::map< int, Module >::iterator iter = modules.begin(); 
         iter!=modules.end(); ++iter )
    {
        iter->second.GetPlugin()->SetID( iter->first );
        nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
            iter->second.GetPlugin()->GetVEModel(), "veModel" ) );
    }

   //Write out the veUser info for the local user
   VE_XML::UserPtr userInfo = new VE_XML::User();
   userInfo->SetUserId( "User" );
   userInfo->SetControlStatus( VE_XML::User::VEControlStatus( "MASTER" ) );
   VE_XML::StateInfoPtr colorState = new VE_XML::StateInfo();
   ///Load the current preferences from the data buffer
   std::map< std::string, VE_XML::CommandWeakPtr > tempMap = 
       UserPreferencesDataBuffer::instance()->GetCommandMap();
   for( std::map< std::string, VE_XML::CommandWeakPtr >::iterator prefIter = 
        tempMap.begin(); prefIter != tempMap.end(); ++prefIter )
   {
      colorState->AddState( prefIter->second );
   }
   userInfo->SetStateInfo( colorState );
   
   nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
        &(*userInfo), "User" ) );
   
   VE_XML::XMLReaderWriter netowrkWriter;
   netowrkWriter.UseStandaloneDOMDocumentManager();
   netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );

   return fileName;
}
////////////////////////////////////////////////////////
void Network::New( bool promptClearXplorer )
{
   // Just clear the design canvas
   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
   
   int answer = wxID_NO;
   if ( !promptClearXplorer )
   {
      wxMessageDialog promptDlg( this, 
                                 _("Do you want to reset Xplorer?"), 
                                 _("Reset Xplorer Warning"), 
                                 wxYES_NO|wxNO_DEFAULT|wxICON_QUESTION, 
                                 wxDefaultPosition);
      answer = promptDlg.ShowModal();
   }
   
   if ( ( answer == wxID_OK ) || ( promptClearXplorer ) )
   {
      std::map<int, Module>::iterator iter;
      for ( iter=modules.begin(); iter!=modules.end(); ++iter )
      {
         VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair(  std::string("UNSIGNED INT") );
         dataValuePair->SetDataName( "Object ID" );
         dataValuePair->SetDataValue( static_cast< unsigned int >( iter->first ) );
         VE_XML::Command* veCommand = new VE_XML::Command();
         veCommand->SetCommandName( std::string("DELETE_OBJECT_FROM_NETWORK") );
         veCommand->AddDataValuePair( dataValuePair );
         bool connected = VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
         //Clean up memory
         delete veCommand;
      }
   }
   
    //Pop the link event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Link >::iterator 
        iter=links.begin(); iter!=links.end(); iter++ )
    {
        RemoveEventHandler( &(*iter) );
    }
    links.clear();

    //Pop the plugin event handlers to clear these event handlers
    for( std::map< int, Module >::iterator iter = modules.begin(); 
        iter!=modules.end(); iter++)
    {
        RemoveEventHandler( iter->second.GetPlugin() );
    }
    modules.clear();

    //Pop the tag event handlers to clear these event handlers
    for( std::vector< VE_Conductor::GUI_Utilities::Tag >::iterator 
         iter =  tags.begin(); iter != tags.end(); iter++ )
    {
        RemoveEventHandler( &(*iter) );
    }
    tags.clear();
    
    ///Reset the canvas available spaces
    sbboxes.clear();
   
    while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);

    Refresh( true );
}
////////////////////////////////////////////////////////
void Network::Load( std::string xmlNetwork, bool promptClearXplorer )
{
   //Get a new canvas first to cleanup memory
   this->New( promptClearXplorer );
   //Now...lets process some files
   _fileProgress = new wxProgressDialog(_("Translation Progress"),
                  _("Load..."), 
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
   delete _fileProgress;

}
////////////////////////////////////////////////////////
void Network::CreateNetwork( std::string xmlNetwork )
{
   if ( xmlNetwork.empty() )
   {
      return;
   }
   
   // Just clear the design canvas
   //while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR) { ; }
   // Start the busy cursor
   // Load from the nt file loaded through wx
   // Get a list of all the command elements   
   _fileProgress->Update( 10, _("start loading") );
    VE_Conductor::XMLDataBufferEngine::instance()->LoadVESData( xmlNetwork );
   _fileProgress->Update( 15, _("start loading") );
   _fileProgress->Update( 25, _("start loading") );

   // do this for network
   veNetwork = VE_Conductor::XMLDataBufferEngine::instance()->GetXMLNetworkDataObject( "Network" );
   
    // we are expecting that a network will be found
    /*if( !objectVector.empty() )
    {
        veNetwork = dynamic_cast< VE_XML::VE_Model::Network* >( objectVector.at( 0 ) );
        objectVector.erase( objectVector.begin() );
    }
    else
    {
        wxMessageBox( _("Improperly formated ves file."), 
                        _("VES File Read Error"), wxOK | wxICON_INFORMATION );
    }*/

//This is needed because on windows the scale must be 1 for the
//wxAutoBufferedPaintDC to work properly
#ifndef WIN32
   _fileProgress->Update( 30, _("start loading") );
   long int tempScaleInfo;
   veNetwork.GetDataValuePair( 0 )->GetData( (userScale.first)  );
   veNetwork.GetDataValuePair( 1 )->GetData( (userScale.second) );
   veNetwork.GetDataValuePair( 2 )->GetData( tempScaleInfo );
   numPix.first = tempScaleInfo;
   veNetwork.GetDataValuePair( 3 )->GetData( tempScaleInfo );
   numPix.second = tempScaleInfo;
   veNetwork.GetDataValuePair( 4 )->GetData( tempScaleInfo );
   numUnit.first = tempScaleInfo;
   veNetwork.GetDataValuePair( 5 )->GetData( tempScaleInfo );
   numUnit.second = tempScaleInfo;
#endif
   _fileProgress->Update( 35, _("start loading") );

    //Setup the links
    for( size_t i = 0; i < veNetwork.GetNumberOfLinks(); ++i )
    {
        links.push_back( VE_Conductor::GUI_Utilities::Link( this ) );
        links.at( i ).SetDCScale( &userScale );
        
        links.at( i ).SetFromPort( *(veNetwork.GetLink( i )->GetFromPort()) );
        links.at( i ).SetToPort( *(veNetwork.GetLink( i )->GetToPort()) );

        long moduleID;
        veNetwork.GetLink( i )->GetFromModule()->GetData( moduleID );
        links.at( i ).SetFromModule( moduleID );
        veNetwork.GetLink( i )->GetToModule()->GetData( moduleID );
        links.at( i ).SetToModule( moduleID );

        size_t numberOfPoints = veNetwork.GetLink( i )->GetNumberOfLinkPoints();
        for ( size_t j = 0; j < numberOfPoints; ++j )
        {
            std::pair< unsigned int, unsigned int > rawPoint = 
                veNetwork.GetLink( i )->GetLinkPoint( j )->GetPoint();
            wxPoint point;
            point.x = rawPoint.first;
            point.y = rawPoint.second;
            links.at( i ).SetPoint( &point );
        }
        // Create the polygon for links
        links.at( i ).CalcLinkPoly();

        links.at(i).SetName( wxString( 
            veNetwork.GetLink( i )->GetLinkName().c_str(), wxConvUTF8) );
        links.at(i).SetUUID( veNetwork.GetLink( i )->GetID() );

        //CORBAServiceList* serviceList = VE_Conductor::CORBAServiceList::instance();
        //serviceList->GetMessageLog()->SetMessage( "velinks:_ " );
        //serviceList->GetMessageLog()->SetMessage( veNetwork.GetLink( i )->GetLinkName().c_str() );
        //serviceList->GetMessageLog()->SetMessage( "_\n" );
        //serviceList->GetMessageLog()->SetMessage( "links:_ " );
        //serviceList->GetMessageLog()->SetMessage( ConvertUnicode( links[i].GetName().c_str() ).c_str() );
        //serviceList->GetMessageLog()->SetMessage( "_\n" );
    }

    for( size_t i = 0; i < veNetwork.GetNumberOfLinks(); ++i )
    {
        PushEventHandler( &links.at( i ) );
    }

    //Setup the tags
    for( size_t i = 0; i < veNetwork.GetNumberOfTags(); ++i )
    {
        tags.push_back( VE_Conductor::GUI_Utilities::Tag( this ) );
        tags.at( i ).SetVETagPtr( veNetwork.GetTag( i ) );
        // Create the polygon for tags
        tags.at( i ).CalcTagPoly();
    }
    
    for( size_t i = 0; i < veNetwork.GetNumberOfTags(); ++i )
    {
        PushEventHandler( &tags.at( i ) );
    }
    

    _fileProgress->Update( 50, _("create models") );
    _fileProgress->Update( 75, _("done create models") );
    // now lets create a list of them
    std::vector< std::string > networkModelVector;
    std::vector< std::string >::iterator stringIter;
    networkModelVector = VE_Conductor::XMLDataBufferEngine::instance()->GetNetworkModelVector( "Network" );
    int timeCalc = 0;
    if(networkModelVector.size())
    {
        timeCalc = 25/networkModelVector.size();
    }
    
    size_t i = 0;
    for( stringIter = networkModelVector.begin(); stringIter != networkModelVector.end(); ++stringIter )
    {
        _fileProgress->Update( 75 + (i*timeCalc), _("Loading data") );
        ++i;
        VE_XML::VE_Model::Model* model = new VE_XML::VE_Model::Model( 
            VE_Conductor::XMLDataBufferEngine::instance()->
            GetXMLModelDataObject( *stringIter ) );

        wxClassInfo* cls = wxClassInfo::FindClass( wxString(model->GetModelName().c_str(),wxConvUTF8) );
        // If the class has not had a custom module been created
        UIPluginBase* tempPlugin = 0;
        if( cls == 0 )
        {
            tempPlugin = new DefaultPlugin();
        }
        else
        {
            tempPlugin = dynamic_cast< UIPluginBase* >( cls->CreateObject() );
        }
        tempPlugin->SetNetworkFrame( this );
        tempPlugin->SetDCScale( &userScale );
        ///Add event handler for the plugins
        PushEventHandler( tempPlugin );
        tempPlugin->SetName( wxString(model->GetModelName().c_str(),wxConvUTF8) );
        tempPlugin->SetCORBAService( VE_Conductor::CORBAServiceList::instance() );
        tempPlugin->SetDialogSize( frame->GetAppropriateSubDialogSize() );
        if ( model->GetIconFilename() != "DefaultPlugin" )
        {   
            tempPlugin->SetImageIcon( model->GetIconFilename(), 
                                   model->GetIconRotation(), 
                                   model->GetIconMirror(), 
                                   model->GetIconScale() );
        }

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
    }

    VE_XML::User userInfo = VE_Conductor::XMLDataBufferEngine::instance()->
        GetXMLUserDataObject( "Network" );
    
    if( !userInfo.GetUserStateInfo() )
    {
        ///Color vector
        std::vector<double> backgroundColor;
        backgroundColor.clear();
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 1.0f );

        VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair( );
        dataValuePair->SetData(std::string("Background Color"),backgroundColor);
        VE_XML::CommandWeakPtr veCommand = new VE_XML::Command();
        veCommand->SetCommandName(std::string("CHANGE_BACKGROUND_COLOR"));
        veCommand->AddDataValuePair(dataValuePair);
        UserPreferencesDataBuffer::instance()->
            SetCommand( std::string("CHANGE_BACKGROUND_COLOR"), veCommand );
    }
    // Create the command and data value pairs
    VE_XML::CommandWeakPtr colorCommand = UserPreferencesDataBuffer::instance()->
        GetCommand( "CHANGE_BACKGROUND_COLOR" );

    VE_Conductor::CORBAServiceList::instance()->
        SendCommandStringToXplorer( colorCommand );

    m_selMod = -1;
    m_selFrPort = -1; 
    m_selToPort = -1; 
    m_selLink = -1; 
    m_selLinkCon = -1; 
    m_selTag = -1; 
    m_selTagCon = -1; 
    xold = yold =0;
    _fileProgress->Update( 100, _("Done") );
    Refresh( true );
}
////////////////////////////////////////////////////////////////////////////////
std::pair< double, double >* Network::GetUserScale( void )
{
   return &userScale;
}
////////////////////////////////////////////////////////////////////////////////
std::pair< unsigned int, unsigned int >* Network::GetNumPix( void )
{
   return &numPix;
}
////////////////////////////////////////////////////////////////////////////////
std::pair< unsigned int, unsigned int >* Network::GetNumUnit( void )
{
   return &numUnit;
}
////////////////////////////////////////////////////////////////////////////////
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
      int index = -1;
      for ( size_t i = 0; i < ports.size(); ++i )
      {
         /*std::cout << "this module id " << moduleID << " " << portNumber << " " 
                  << i << " " << ports.at( i ).GetPortNumber() << " " 
                  << ports[ i ].GetPortLocation()->GetPoint().first << " " 
                  << ports[ i ].GetPortLocation()->GetPoint().second << std::endl;*/
         if ( ports.at( i ).GetPortNumber() == portNumber )
         {
            index = i;
            break;
         }
      }
      
      if ( index == -1 )
      {
         std::ostringstream msg;
         msg << "Could not find port " << portNumber << " in module " << moduleID << std::endl;
         CORBAServiceList* serviceList = VE_Conductor::CORBAServiceList::instance();
         serviceList->GetMessageLog()->SetMessage( msg.str().c_str() );
         index = 0;
      }
      
      /*std::cout << portNumber << " " 
        << ports[ index ].GetPortLocation()->GetPoint().first << " " 
        << ports[ index ].GetPortLocation()->GetPoint().second << std::endl;*/
      wxPoint portPoint( ports[ index ].GetPortLocation()->GetPoint().first, ports[ index ].GetPortLocation()->GetPoint().second );
      tempPoint.x = bbox.x+portPoint.x;
      tempPoint.y = bbox.y+portPoint.y;
   }

   return tempPoint;
}
////////////////////////////////////////////////////////////////////////////////
void* Network::Entry( void )
{
   isLoading = true;
   this->CreateNetwork( tempXMLNetworkData );
   isLoading = false;
   return 0;
}
////////////////////////////////////////////////////////////////////////////////
void Network::SetIDOnAllActiveModules( void )
{
   CORBAServiceList* serviceList = VE_Conductor::CORBAServiceList::instance();

   std::map< int, Module >::iterator iter;
   for ( iter=modules.begin(); iter!=modules.end(); ++iter )
   {
      std::string moduleName = ConvertUnicode( iter->second.GetPlugin()->GetName().c_str() );//GetClassName();
      int moduleId = iter->first;
      serviceList->SetID( moduleId, moduleName );
   }
}
////////////////////////////////////////////////////////////////////////////////
bool Network::IsDragging()
{
	return dragging;
}
////////////////////////////////////////////////////////////////////////////////
void Network::SetSelectedModule(int mod)
{
	m_selFrPort = -1; 
	m_selToPort = -1; 
	m_selLink = -1; 
	m_selLinkCon = -1; 
	m_selTag = -1; 
	m_selTagCon = -1; 
	m_selMod = mod;
    modules[m_selMod].GetPlugin()->SetHighlightFlag( true );
	Refresh(true);
	///Update();
}
