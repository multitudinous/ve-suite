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

    VESKeyPressEater(QObject *parent = 0)
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
    
    bool ProcessEvent( QObject *obj, QEvent *event)
    {
        m_app->AcquireQtLock();
        bool temp = QObject::eventFilter(obj, event);
        //bool temp = obj->eventFilter( obj, event);
        //bool consumed = obj->event(event);
        //event->spont = false;     
        m_app->ReleaseQtLock();
        std::cout << " consumed " << temp << std::endl << std::flush;
        return temp;   
    }

    bool eventFilter(QObject *obj, QEvent *event)
    {
        if( event->type() == QEvent::Paint )
        {
            bool temp = QObject::eventFilter(obj, event);
            return temp;
        }

        if( event->type() == QEvent::Timer )
        {
            bool temp = QObject::eventFilter(obj, event);
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
        
        bool temp = QObject::eventFilter(obj, event);
        return temp;
    }
};
