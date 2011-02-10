#include "VESQtApplication.h"
#include <iostream>
#include <QtCore/QEvent>
#include <QtGui/QKeyEvent>

VESQtApplication::VESQtApplication(int & argc, char ** argv, ves::xplorer::App* app ):
    QApplication(argc, argv),
    m_app( app )
{
    ;
}

bool VESQtApplication::notify(QObject* obj, QEvent* event)
{
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
    }*/
    
    
    //std::cout << event->type() << std::endl << std::flush;
    
    //return ProcessEvent( obj, event );
    
    bool temp = QApplication::notify( obj, event );
    return temp;
    
    //if( m_signalLock.try_lock() )
    {
        // Process anything in the queue
        /*while( !m_queue.empty() )
        {
            std::pair<QObject*, QEvent*> rePair = m_queue.front();
            (rePair->first)->event( rePair->second );
            m_queue.pop;
        }*/

        // Now process this latest event
        std::cout << "Sending an event to receiver " << obj << std::endl << std::flush;
        obj->event(event);
        //m_signalLock.release();

        return true;
    }
    //else
    {
        // Add recv and evt to queue
        // std::pair<QObject*, QEvent*> rePair(recv, evt);
        // m_queue.push(rePair);
        return true;
    }
}

bool VESQtApplication::ProcessEvent( QObject *obj, QEvent *event)
{
    /*if( m_app->AcquireQtLock() )
    {
        while( !m_queue.empty() )
        {
            std::pair<QObject*, QEvent*> rePair = m_queue.front();
            QApplication::notify( rePair.first, rePair.second );
            m_queue.pop();
        }       
        m_app->ReleaseQtLock();
        return true;
    }
    else
    {
        // Add recv and evt to queue
        std::pair<QObject*, QEvent*> rePair( obj, event);
        m_queue.push(rePair);
        return true;
    }*/

    //std::cout << " first " << event->type() << " " << m_queue.size() << std::endl << std::flush;
    std::pair<QObject*, QEvent*> rePair( obj, event);
    m_queue.push(rePair);

    if( m_queue.size() == 1 )
    {
        m_app->AcquireQtLock();
    }
    /*else
    {
        // Add recv and evt to queue
        std::pair<QObject*, QEvent*> rePair( obj, event);
        m_queue.push(rePair);
    }*/

    // Add recv and evt to queue
    //std::pair<QObject*, QEvent*> rePair( obj, event);
    //m_queue.push(rePair);
    //std::pair<QObject*, QEvent*> rePair1 = m_queue.back();
    /*try
    {
        m_app->AcquireQtLock();
    }
    catch( ... )
    {
        std::cout << " exception " << std::endl << std::flush;
    }*/
    bool temp = QApplication::notify( obj, event );

    if( m_queue.size() == 1 )
    {
        m_app->ReleaseQtLock();
    }
    //else
    {
        m_queue.pop();
    }

    //m_app->ReleaseQtLock();
    //std::cout << " consumed " << temp << std::endl << std::flush;
    return temp;   
}
