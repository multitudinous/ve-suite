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

/** This class is a slightly altered version of the FileEdit class shown in
 Qt Quarterly at
http://doc.qt.nokia.com/qq/qq18-propertybrowser.html#extendingtheframework
It is being used in accordance with the terms of LGPL **/


#include <qtpropertybrowser.h>
#include <QtCore/QMap>

namespace ves
{
namespace conductor
{

class FilePathManager : public QtAbstractPropertyManager
{
    Q_OBJECT
public:
    FilePathManager(QObject *parent = 0)
        : QtAbstractPropertyManager(parent)
            { }

    QString value(const QtProperty *property) const;
    QString filter(const QtProperty *property) const;

public Q_SLOTS:
    void setValue(QtProperty *property, const QString &val);
    void setFilter(QtProperty *property, const QString &fil);
Q_SIGNALS:
    void valueChanged(QtProperty *property, const QString &val);
    void filterChanged(QtProperty *property, const QString &fil);
protected:
    virtual QString valueText(const QtProperty *property) const { return value(property); }
    virtual void initializeProperty(QtProperty *property) { theValues[property] = Data(); }
    virtual void uninitializeProperty(QtProperty *property) { theValues.remove(property); }
private:

    struct Data
    {
        QString value;
        QString filter;
    };

    QMap<const QtProperty *, Data> theValues;
};

}
}
