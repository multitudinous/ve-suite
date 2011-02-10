#pragma once

#define QT_NO_KEYWORDS
#include "App.h"

#include <QtGui/QApplication>
#include <queue>


class VESQtApplication : public QApplication
{
public:
    VESQtApplication(int & argc, char ** argv, ves::xplorer::App* app );

protected:
    virtual bool notify(QObject* recv, QEvent* evt);

    bool ProcessEvent( QObject *obj, QEvent *event);

private:
    ves::xplorer::App* m_app;
    
    std::queue< std::pair<QObject*, QEvent*> > m_queue;

};

