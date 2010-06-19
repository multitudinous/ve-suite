#pragma once

#include <QtCore/QThread>

class nonQtMainThread : public QThread
{
public:
    nonQtMainThread( int argc, char* argv[] );
    virtual ~nonQtMainThread( );
    
    void run();
private:
    int mArgc;
    char** mArgv;
};
