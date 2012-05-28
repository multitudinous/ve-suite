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

#ifndef __VES_CONDUCTOR_QT_MINERVA_STACKED_WIDGET_H__
#define __VES_CONDUCTOR_QT_MINERVA_STACKED_WIDGET_H__

#include <QtGui/QStackedWidget>

namespace Minerva
{
namespace Core
{
namespace Data
{
class Feature;
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

class LayersTree;
class AddLayerWidget;

class StackedWidget : public QStackedWidget
{
    Q_OBJECT;

    typedef QStackedWidget BaseClass;
public:

    StackedWidget( QWidget* parent = 0x0 );
    virtual ~StackedWidget();

    void setFeature( Minerva::Core::Data::Feature* feature );

protected Q_SLOTS:

    void showAddLayerWidget();
    void addLayerWidgetAccepted();
    void addLayerWidgetRejected();

private:

    LayersTree* m_layersTree;
    AddLayerWidget* m_addLayerWidget;
};

}
}
}
}

#endif // __VES_CONDUCTOR_QT_MINERVA_STACKED_WIDGET_H__
