/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#pragma once
#if defined( _DARWIN )

#define QT_NO_KEYWORDS
#include "App.h"

#include <QtGui/QApplication>
#include <queue>

/*!\file VESQtApplication.h
 * \class ves::xplorer::VESQtApplication
 * \namepsace ves::xplorer
 * VESQtApplication API
 * http://qt-project.org/doc/qt-4.8/developing-on-mac.html
 */
namespace ves
{
namespace xplorer
{
class VESQtApplication : public QApplication
{
public:
    ///Our special constructor
    VESQtApplication( int& argc, char** argv, ves::xplorer::App* app );

protected:
    ///Override the default notify function so that we can sync things
    virtual bool notify( QObject* recv, QEvent* evt );
    ///Convenience function to process the actual event with QApplication::notify
    bool ProcessEvent( QObject* obj, QEvent* event );

private:
    ///Provides access to the mutexs to lock event processing to the draw thread
    ves::xplorer::App* m_app;
    ///A queue to keep track of how many events we have recursed through
    std::queue< std::pair<QObject*, QEvent*> > m_queue;
};
}
}
#endif

