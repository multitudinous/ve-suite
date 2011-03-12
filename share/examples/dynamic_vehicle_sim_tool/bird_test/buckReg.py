from js import *
from math import *

def getLocalUVAxes(pt1, pt2, pt3):
	centroid = ((pt1[0]+pt2[0]+pt3[0])/3.0, \
		    (pt1[1]+pt2[1]+pt3[1])/3.0, \
		    (pt1[2]+pt2[2]+pt3[2])/3)
	
	v12 = (pt2[0]-pt1[0],pt2[1]-pt1[1],pt2[2]-pt1[2])
	v13 = (pt3[0]-pt1[0],pt3[1]-pt1[1],pt3[2]-pt1[2])

	normal = (v12[1]*v13[2]-v12[2]*v13[1], \
		  v12[2]*v13[0]-v12[0]*v13[2], \
		  v12[0]*v13[1]-v12[1]*v13[0])
	mag = sqrt(normal[0]*normal[0]+normal[1]*normal[1]+normal[2]*normal[2])
	unormal = (normal[0]/mag,normal[1]/mag,normal[2]/mag)

	nzaxis = (pt1[0]-centroid[0],pt1[1]-centroid[1],pt1[2]-centroid[2])
	mag = sqrt(nzaxis[0]*nzaxis[0]+nzaxis[1]*nzaxis[1]+nzaxis[2]*nzaxis[2])
	unzaxis = (nzaxis[0]/mag,nzaxis[1]/mag,nzaxis[2]/mag)
	uzaxis = (-unzaxis[0],-unzaxis[1],-unzaxis[2])

	uxaxis = (unormal[1]*uzaxis[2]-unormal[2]*uzaxis[1], \
		  unormal[2]*uzaxis[0]-unormal[0]*uzaxis[2], \
		  unormal[0]*uzaxis[1]-unormal[1]*uzaxis[0])

	return centroid, uxaxis, unormal, uzaxis

def getRotAngles(xaxis, yaxis, zaxis):
	roty = atan2(zaxis[0],sqrt(zaxis[1]*zaxis[1]+zaxis[2]*zaxis[2]))
	cosy = cos(roty)
	rotx = atan2(zaxis[1]/-cosy,zaxis[2]/cosy)
	rotz = atan2(yaxis[0]/-cosy,xaxis[0]/cosy)

	return rotx, roty, rotz

def getRotToOrigin(pt, rx, ry, rz):
	newpt = (pt[0]*cos(rz)*cos(ry) + \
		pt[1]*(cos(rz)*sin(ry)*sin(rx)-sin(rz)*cos(rx)) + \
		pt[2]*(cos(rz)*sin(ry)*cos(rx)+sin(rz)*sin(rx)), \
		pt[0]*sin(rz)*cos(ry) + \
		pt[1]*(sin(rz)*sin(ry)*sin(rx)+cos(rz)*cos(rx)) + \
		pt[2]*(sin(rz)*sin(ry)*cos(rx)-cos(rz)*sin(rx)), \
		-pt[0]*sin(ry) + pt[1]*cos(ry)*sin(rx) + pt[2]*cos(ry)*cos(rx))

	return newpt

def getRotFromOrigin(pt, rx, ry, rz):
	newpt = (pt[0]*cos(ry)*cos(rz) - pt[1]*cos(ry)*sin(rz) + pt[2]*sin(ry), \
		pt[0]*(cos(rx)*sin(rz)+sin(rx)*sin(ry)*cos(rz)) + \
		pt[1]*(cos(rx)*cos(rz)-sin(rx)*sin(ry)*sin(rz)) - \
		pt[2]*sin(rx)*cos(ry), \
		pt[0]*(sin(rx)*sin(rz)-cos(rx)*sin(ry)*cos(rz)) + \
		pt[1]*(sin(rx)*cos(rz)+cos(rx)*sin(ry)*sin(rz)) + \
		pt[2]*cos(rx)*cos(ry))

	return newpt

def registerBuck(sipx, sipy, sipz, rrbird, fbird, lrbird):
	sip = sipx, sipy, sipz

	print fbird
	print lrbird
	print rrbird

	fbirdd = (sip[0]*u.mm-1048.1*u.mm,sip[1]*u.mm+686.8*u.mm,sip[2]*u.mm+13.3*u.mm)
	lrbirdd = (sip[0]*u.mm+597.8*u.mm,sip[1]*u.mm+792.5*u.mm,sip[2]*u.mm+421.4*u.mm)
	rrbirdd = (sip[0]*u.mm+600.9*u.mm,sip[1]*u.mm+792.4*u.mm,sip[2]*u.mm-421.4*u.mm)

	print fbirdd
	print lrbirdd
	print rrbirdd

	if (fbird == 0):
		fbirda = scene.hmd.GetLocation().trans()
	else:
		exec "fbirda=scene.bird"+str(fbird)+".GetLocation().trans()"
	if (lrbird == 0):
		lrbirda = scene.hmd.GetLocation().trans()
	else:
		exec "lrbirda=scene.bird"+str(lrbird)+".GetLocation().trans()"
	if (rrbird == 0):
		rrbirda = scene.hmd.GetLocation().trans()
	else:
		exec "rrbirda=scene.bird"+str(rrbird)+".GetLocation().trans()"

	print fbirda
	print lrbirda
	print rrbirda

	cd,xd,yd,zd = getLocalUVAxes(fbirdd,lrbirdd,rrbirdd)
	ca,xa,ya,za = getLocalUVAxes(fbirda,lrbirda,rrbirda)
	rotxd,rotyd,rotzd = getRotAngles(xd,yd,zd)
	rotxa,rotya,rotza = getRotAngles(xa,ya,za)
	print "rotd",rotxd*180.0/3.14159,rotyd*180.0/3.14159,rotzd*180.0/3.14159
	print "rota",rotxa*180.0/3.14159,rotya*180.0/3.14159,rotza*180.0/3.14159

	fobpos = scene.FOB_trans_icon.GetLocation()
	ta = (xyz(rotxa,rotya,rotza)*trans(ca[0],ca[1],ca[2]))
	td = (xyz(rotxd,rotyd,rotzd)*trans(cd[0],cd[1],cd[2]))
	tai = ta.Inverse()

	temp = tai*td
	newtrans = fobpos*temp

	scene.FOB_trans_icon.SetLocation(newtrans)