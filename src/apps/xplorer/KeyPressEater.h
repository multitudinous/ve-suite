#pragma once

#define QT_NO_KEYWORDS

#include <QtCore/QObject>
#include <QtCore/QEvent>
#include <QtGui/QKeyEvent>

#include <iostream>

class VESKeyPressEater : public QObject
{
    Q_OBJECT
public:
    VESKeyPressEater()
    {
        ;
    }

    VESKeyPressEater(QObject *parent = 0)
        :
        QObject( parent )
    {
        ;
    }

    virtual ~VESKeyPressEater()
    {
        ;
    }

protected:
    bool eventFilter(QObject *obj, QEvent *event)
    {
        if( event->type() == QEvent::KeyPress )
        {
            QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
            //qDebug("Ate key press %d", keyEvent->key());
            std::cout << " Ate key press " << keyEvent->key() << std::endl << std::flush;
            return true;
        }
        else
        {
            // standard event processing
            std::cout << " standard process " << std::endl << std::flush;
            return QObject::eventFilter(obj, event);
        }
    }
};
