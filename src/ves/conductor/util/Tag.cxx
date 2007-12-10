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
#ifdef WIN32
#include <wx/msw/winundef.h>
#endif

#include <wx/scrolwin.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/dcclient.h>

#include <ves/conductor/util/Tag.h>

#include <ves/open/xml/model/Tag.h>
#include <ves/open/xml/model/Point.h>

using namespace ves::conductor::util;

////////////////////////////////////////////////
Tag::Tag( wxScrolledWindow* designCanvas )
{
    canvas = designCanvas;
    ves::open::xml::model::Tag temp;
    uuid = temp.GetID();
}
////////////////////////////////////////////////
Tag::~Tag( void )
{
    ;//Do nothing since this class doesn't manage any of the pointers memory
}
////////////////////////////////////////////////
Tag::Tag( const Tag& input )
{
    cons[0] = input.cons[0];
    cons[1] = input.cons[1];
    text = input.text;
    box = input.box;
    poly = input.poly;
    canvas = input.canvas;
    uuid = input.uuid;
}
////////////////////////////////////////////////
Tag& Tag::operator= ( const Tag& input )
{
    if( this != &input )
    {
        cons[0] = input.cons[0];
        cons[1] = input.cons[1];
        text = input.text;
        box = input.box;
        poly = input.poly;
        canvas = input.canvas;
        uuid = input.uuid;
    }
    return *this;
}
////////////////////////////////////////////////
wxPoint* Tag::GetConnectorsPoint( size_t i )
{
    return &( cons[i] );
}
////////////////////////////////////////////////
wxString* Tag::GetTagText( void )
{
    return &( text );
}
////////////////////////////////////////////////
ves::conductor::util::Polygon* Tag::GetPolygon( void )
{
    return &( poly );
}
////////////////////////////////////////////////
wxRect* Tag::GetBoundingBox( void )
{
    return &( box );
}

////////////////////////////////////////////////////
void Tag::CalcTagPoly( void )
{
    // Create a poly based on a tag
    wxPoint endpos;
    endpos.x = box.x;
    endpos.y = box.y + box.height / 2;

    // first point of the extension line
    poly.GetPolygon()->push_back( wxPoint( cons[0].x, cons[0].y - 3 ) );
    // second point of the extension line
    poly.GetPolygon()->push_back( wxPoint( cons[1].x, cons[1].y - 3 ) );
    // first corner of the text box
    poly.GetPolygon()->push_back( wxPoint( endpos.x, endpos.y - 3 ) );
    poly.GetPolygon()->push_back( wxPoint( box.x, box.y ) );
    poly.GetPolygon()->push_back( wxPoint( box.x + box.width, box.y ) );
    poly.GetPolygon()->push_back( wxPoint( box.x + box.width, box.y + box.height ) );
    poly.GetPolygon()->push_back( wxPoint( box.x, box.y + box.height ) );
    // and now back around again
    poly.GetPolygon()->push_back( wxPoint( endpos.x, endpos.y + 3 ) );
    poly.GetPolygon()->push_back( wxPoint( cons[1].x, cons[1].y + 3 ) );
    poly.GetPolygon()->push_back( wxPoint( cons[0].x, cons[0].y + 3 ) );
}

////////////////////////////////////////////////////////////
void Tag::DrawTagCon( bool flag, std::pair< double, double > scale )
{
    wxClientDC dc( canvas );
    canvas->PrepareDC( dc );
    dc.SetUserScale( scale.first, scale.second );

    wxBrush old_brush = dc.GetBrush();
    wxPen old_pen = dc.GetPen();

    if( flag )
    {
        dc.SetBrush( *wxGREEN_BRUSH );
        dc.SetPen( *wxBLACK_PEN );
    }
    else
    {
        dc.SetBrush( *wxWHITE_BRUSH );
        dc.SetPen( *wxWHITE_PEN );
    }

    wxPoint bport[4];
    bport[0] = wxPoint( 0, 0 );
    bport[1] = wxPoint( 10, 0 );
    bport[2] = wxPoint( 10, 10 );
    bport[3] = wxPoint( 0, 10 );

    for( size_t i = 0; i < 2; ++i )
    {
        wxCoord xoff = cons[i].x - 3;
        wxCoord yoff = cons[i].y - 3;

        dc.DrawPolygon( 4, bport, xoff, yoff );
    }

    dc.SetBrush( old_brush );
    dc.SetPen( old_pen );
}
////////////////////////////////////////////////////////////////////////////////
void Tag::DrawTag( bool flag, wxDC& dc, std::pair< double, double > scale )
{
    //wxClientDC dc( canvas );
    //canvas->PrepareDC(dc);
    dc.SetUserScale( scale.first, scale.second );

    wxBrush old_brush = dc.GetBrush();
    wxPen old_pen = dc.GetPen();
    wxPen mypen( old_pen );
    mypen.SetColour( _( "BLUE" ) );
    //  mypen.SetStyle(wxDOT);
    // mypen.SetStyle(wxLONG_DASH);

    int w, h;
    dc.GetTextExtent( text, &w, &h );
    box.width = w;
    box.height = h;

    CalcTagPoly();
    wxPoint points[3];
    points[0] = cons[0];
    points[1] = cons[1];
    points[2] = wxPoint( box.x,  box.y + box.height / 2 );


    if( !flag )
    {
        dc.SetPen( *wxWHITE_PEN );
        dc.SetBrush( *wxWHITE_BRUSH );
    }
    else
    {
        dc.SetPen( mypen );
        dc.SetBrush( *wxWHITE_BRUSH );
    }

    dc.DrawLines( 3, points );
    dc.DrawRectangle( box.x - 3,  box.y - 3,  box.width + 6,  box.height + 6 );

    if( flag )
        dc.DrawText( text, box.x, box.y );

    dc.SetPen( old_pen );
    dc.SetBrush( old_brush );
}
////////////////////////////////////////////////////////////////////////////////
void Tag::SetVETagPtr( ves::open::xml::model::TagPtr inputTag )
{
    text = wxString( inputTag->GetText().c_str(), wxConvUTF8 );
    cons[0].x = inputTag->GetPoint( 0 )->GetPoint().first;
    cons[0].y = inputTag->GetPoint( 0 )->GetPoint().second;
    cons[1].x = inputTag->GetPoint( 1 )->GetPoint().first;
    cons[1].y = inputTag->GetPoint( 1 )->GetPoint().second;
    box.x = inputTag->GetPoint( 2 )->GetPoint().first;
    box.x = inputTag->GetPoint( 2 )->GetPoint().second;
    uuid = inputTag->GetID();
}
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::model::TagPtr Tag::GetVETagPtr()
{
    ves::open::xml::model::TagPtr tagPtr = new ves::open::xml::model::Tag();
    tagPtr->SetText( ConvertUnicode( text.c_str() ) );
    tagPtr->SetID( uuid );
    //Order is important below
    ves::open::xml::model::PointPtr con0Ptr = new ves::open::xml::model::Point();
    std::pair< unsigned int, unsigned int > point0;
    point0.first = cons[0].x;
    point0.second = cons[0].y;
    con0Ptr->SetPoint( point0 );
    tagPtr->AddPoint( con0Ptr );
    ves::open::xml::model::PointPtr con1Ptr = new ves::open::xml::model::Point();
    std::pair< unsigned int, unsigned int > point1;
    point1.first = cons[1].x;
    point1.second = cons[1].y;
    con1Ptr->SetPoint( point1 );
    tagPtr->AddPoint( con1Ptr );
    ves::open::xml::model::PointPtr con2Ptr = new ves::open::xml::model::Point();
    std::pair< unsigned int, unsigned int > point2;
    point2.first = box.x;
    point2.second = box.y;
    con2Ptr->SetPoint( point2 );
    tagPtr->AddPoint( con2Ptr );
    return tagPtr;
}
////////////////////////////////////////////////////////////////////////////////
