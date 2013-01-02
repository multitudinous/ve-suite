/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#pragma once

#define QT_NO_KEYWORDS

#include <QtCore/QObject>
#include <QtCore/QEvent>
#include <QtGui/QKeyEvent>

#include <iostream>

#include "App.h"

class VESKeyPressEater : public QObject
{
    Q_OBJECT
public:
    VESKeyPressEater()
        :
        QObject( 0 ),
        m_app( 0 ),
        m_paint( true )
    {
        ;
    }

    VESKeyPressEater( QObject* parent = 0 )
        :
        QObject( parent ),
        m_app( 0 ),
        m_paint( true )
    {
        ;
    }

    virtual ~VESKeyPressEater()
    {
        ;
    }

    void SetApp( ves::xplorer::App* app )
    {
        m_app = app;
    }

protected:
    ves::xplorer::App* m_app;
    bool m_paint;

    bool ProcessEvent( QObject* obj, QEvent* event )
    {
#if defined( VES_QT_APP )
        m_app->AcquireQtLock();
#endif
        bool temp = QObject::eventFilter( obj, event );
        //bool temp = obj->eventFilter( obj, event);
        //bool consumed = obj->event(event);
        //event->spont = false;
#if defined( VES_QT_APP )
        m_app->ReleaseQtLock();
#endif
        std::cout << " consumed " << temp << std::endl << std::flush;
        return temp;
    }

    bool eventFilter( QObject* obj, QEvent* event )
    {
        if( event->type() == QEvent::Paint )
        {
            bool temp = QObject::eventFilter( obj, event );
            return temp;
        }

        if( event->type() == QEvent::Timer )
        {
            bool temp = QObject::eventFilter( obj, event );
            return temp;
        }

        if( event->type() == QEvent::None )
        {
            return ProcessEvent( obj, event );
        }

        //Look for the various event types in qcoreevent.h
        if( event->type() == QEvent::KeyPress )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::KeyRelease )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::MouseButtonPress )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::MouseButtonRelease )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::GraphicsSceneMousePress )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::GraphicsSceneMouseRelease )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::MetaCall )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::NonClientAreaMouseButtonDblClick )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::MouseButtonDblClick )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::DeferredDelete )
        {
            return ProcessEvent( obj, event );
        }

        if( event->type() == QEvent::LayoutRequest )
        {
            return ProcessEvent( obj, event );
        }


        std::cout << event->type() << std::endl << std::flush;

        //return ProcessEvent( obj, event );

        bool temp = QObject::eventFilter( obj, event );
        return temp;
    }
};
