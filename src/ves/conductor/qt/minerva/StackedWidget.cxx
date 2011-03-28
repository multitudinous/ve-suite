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
#include <ves/conductor/qt/minerva/StackedWidget.h>
#include <ves/conductor/qt/minerva/LayersTree.h>
#include <ves/conductor/qt/minerva/AddLayerWidget.h>

#include <Minerva/Core/Data/Container.h>

using namespace ves::conductor::qt::minerva;

StackedWidget::StackedWidget ( QWidget *parent ) : BaseClass ( parent ),
  m_layersTree ( 0x0 ),
  m_addLayerWidget ( 0x0 )
{
    m_layersTree = new LayersTree ( this );
    this->addWidget ( m_layersTree );
    QObject::connect ( m_layersTree, SIGNAL ( addLayerRequested() ), this, SLOT ( showAddLayerWidget() ) );

    m_addLayerWidget = new AddLayerWidget;
    this->addWidget ( m_addLayerWidget );

    QObject::connect ( m_addLayerWidget, SIGNAL ( accepted() ), this, SLOT ( addLayerWidgetAccepted() ) );
    QObject::connect ( m_addLayerWidget, SIGNAL ( rejected() ), this, SLOT ( addLayerWidgetRejected() ) );
}

StackedWidget::~StackedWidget()
{
    delete m_layersTree;
}

void StackedWidget::setFeature ( Minerva::Core::Data::Feature * feature )
{
    m_layersTree->BuildTree ( feature );
}

void StackedWidget::showAddLayerWidget()
{
    this->setCurrentIndex ( 1 );
}

void StackedWidget::addLayerWidgetAccepted()
{
    this->setCurrentIndex ( 0 );

    // Get the added layers and add them to the planet.
    Minerva::Core::Data::Feature::RefPtr feature ( m_layersTree->GetCurrentFeature() );
    if ( feature.valid() )
    {
        Minerva::Core::Data::Container::RefPtr container ( feature->asContainer() );
        if ( container )
        {
            m_addLayerWidget->AddLayersToFeature ( container.get() );
        }
    }
}

void StackedWidget::addLayerWidgetRejected()
{
    this->setCurrentIndex ( 0 );
}

#endif
