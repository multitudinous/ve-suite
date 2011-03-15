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
 * Date modified: $Date: 2011-01-03 22:17:45 -0600 (Mon, 03 Jan 2011) $
 * Version:       $Rev: 15339 $
 * Author:        $Author: mccdo $
 * Id:            $Id: LayersTree.h 15339 2011-01-04 04:17:45Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifdef MINERVA_GIS_SUPPORT
#include <ves/conductor/qt/minerva/AddLayerWidget.h>
#include <ves/conductor/qt/minerva/AddFileSystemStackedWidget.h>

#include <Minerva/Qt/Widgets/AddNetworkLayerWidget.h>

#include "Minerva/Core/Layers/RasterLayerWms.h"

#include "QtGui/QVBoxLayout"
#include "QtGui/QHBoxLayout"
#include "QtGui/QPushButton"
#include "QtGui/QTabWidget"

using namespace ves::conductor::qt::minerva;

typedef Minerva::QtWidgets::AddNetworkLayerWidget<Minerva::Core::Layers::RasterLayerWms> AddWmsLayerWidget;

AddLayerWidget::AddLayerWidget ( QWidget *parent ) : BaseClass ( parent )
{
    QTabWidget *tabs ( new QTabWidget ( this ) );
    QPushButton *ok ( new QPushButton ( "Ok" ) );
    QPushButton *cancel ( new QPushButton ( "Cancel" ) );

    QObject::connect ( ok, SIGNAL ( clicked() ), this, SIGNAL ( accepted() ) );
    QObject::connect ( cancel, SIGNAL ( clicked() ), this, SIGNAL ( rejected() ) );

    QVBoxLayout *topLayout ( new QVBoxLayout );
    this->setLayout ( topLayout );
  
    QVBoxLayout *vLayout ( new QVBoxLayout );
    QHBoxLayout *hLayout ( new QHBoxLayout );
  
    topLayout->addLayout ( vLayout );
    topLayout->addLayout ( hLayout );
  
    vLayout->addWidget ( tabs );
    hLayout->addStretch();
    hLayout->addWidget ( ok );
    hLayout->addWidget ( cancel );

    AddFileSystemStackedWidget* addFileSystemWidget ( new AddFileSystemStackedWidget ( tabs ) );
    tabs->addTab ( addFileSystemWidget, "File System" );

    AddWmsLayerWidget* addWmsLayerWidget ( new AddWmsLayerWidget ( tabs ) );
    tabs->addTab ( addWmsLayerWidget, "WMS" );

    // For now hide the button that opens a dialog.  This is usually not needed.
    addWmsLayerWidget->setViewOptionsVisibility ( false );
}

AddLayerWidget::~AddLayerWidget()
{
}

#endif
