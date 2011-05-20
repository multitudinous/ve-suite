/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#if defined( _DARWIN )
#include "VESQtApplication.h"
#include <iostream>
#include <QtCore/QEvent>
#include <QtGui/QKeyEvent>

namespace ves
{
namespace xplorer
{
////////////////////////////////////////////////////////////////////////////////
VESQtApplication::VESQtApplication(int & argc, char ** argv, ves::xplorer::App* app ):
    QApplication(argc, argv),
    m_app( app )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool VESQtApplication::notify(QObject* obj, QEvent* event)
{
    //Look for the various event types in qcoreevent.h
    if( event->type() == QEvent::Paint )
    {
        bool temp = QApplication::notify( obj, event );
        return temp;
    }
    
    if( event->type() == QEvent::Timer )
    {
        bool temp = QApplication::notify( obj, event );
        return temp;
    }
    
    /*if( event->type() == QEvent::None )
    {
        return ProcessEvent( obj, event );
    }*/
    
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
    
    /*if( event->type() == QEvent::GraphicsSceneMousePress )
    {
        return ProcessEvent( obj, event );
    }
    
    if( event->type() == QEvent::GraphicsSceneMouseRelease )
    {
        return ProcessEvent( obj, event );
    }*/
    
    /*if( event->type() == QEvent::MetaCall )
    {
        return ProcessEvent( obj, event );
    }*/
    
    /*if( event->type() == QEvent::NonClientAreaMouseButtonDblClick )
    {
        return ProcessEvent( obj, event );
    }*/
    
    if( event->type() == QEvent::MouseButtonDblClick )
    {
        return ProcessEvent( obj, event );
    }
    
    /*if( event->type() == QEvent::DeferredDelete )
    {
        return ProcessEvent( obj, event );
    }*/
    
    if( event->type() == QEvent::Leave )
    {
        return ProcessEvent( obj, event );
    }

    //std::cout << event->type() << std::endl << std::flush;
    
    //return ProcessEvent( obj, event );
    
    return QApplication::notify( obj, event );
}
////////////////////////////////////////////////////////////////////////////////
bool VESQtApplication::ProcessEvent( QObject *obj, QEvent *event)
{
    std::pair<QObject*, QEvent*> rePair( obj, event);
    m_queue.push(rePair);

    if( m_queue.size() == 1 )
    {
        m_app->AcquireQtLock();
    }

    bool temp = QApplication::notify( obj, event );

    if( m_queue.size() == 1 )
    {
        m_app->ReleaseQtLock();
    }
    
    m_queue.pop();

    return temp;   
}
////////////////////////////////////////////////////////////////////////////////
}
}
#endif
