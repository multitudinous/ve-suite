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
#ifndef __ADD_FILE_SYSTEM_WIDGET_H__
#define __ADD_FILE_SYSTEM_WIDGET_H__

#include <QtGui/QWidget>

namespace Ui
{
class AddFileSystemWidget;
}

namespace Minerva
{
namespace Core
{
namespace Data
{
class Container;
}
}
}

namespace ves
{
namespace conductor
{
namespace qt
{
namespace minerva
{

class AddFileSystemWidget : public QWidget
{
    Q_OBJECT;

public:

    typedef QWidget BaseClass;

    AddFileSystemWidget( QWidget* parent = 0x0 );

    void AddLayersToFeature( Minerva::Core::Data::Container* );

Q_SIGNALS:

    void showFileDialog();

public Q_SLOTS:

    void onFilesSelected( const QStringList& fileNames );
    void onFileSelected( const QString& fileName );

private Q_SLOTS:
    void on_addFilesButton_clicked();
    void on_removeSelectedFilesButton_clicked();

private:

    Ui::AddFileSystemWidget* _ui;
};

}
}
}
}

#endif // __ADD_FILE_SYSTEM_WIDGET_H__
