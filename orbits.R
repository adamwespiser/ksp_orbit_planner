##http://www.braeunig.us/space/orbmech.htm
## http://www.braeunig.us/space/interpl.htm
pow <<- `^`
rad_print_fmt <- function(rad){
		paste(format(rad,digits=5), " rad is ",format(rad/(1.99*pi) * 360,digits=5), " deg",sep="")

}

Kerbin_GM = 3.5316000*(10^12) #m^3/s^2
Kerbin_SOI_radius = 84159286 #meters
Kerbin_atm_scale_height = 5600 # meters
Kerbin_sync_orbit =			2868.75  * pow(10,3)
Kerbin_radius = 600 * pow(10,3)
KR = Kerbin_radius

Kerbin_semi_sync = list(period = 10774.7,
												alt = 1585.18 * pow(10,3)  + Kerbin_radius,
												orb_vel = 1271.28)

ave_ang_vel <- function(GM,r){
		# GM = w^2 * r^3
		w = sqrt(pow(r,3) / GM)
		w
}

period_from_ang_vel <- function(w){
		# w = 2*pi / P
		w / 2*pi
}
ang_vel_from_period <- function(P){
		(2 * pi) / P
}

period_from_semi_major <- function(GM,a){
		#p^2 = (4pi^2 * r^3) / GM
		sqrt((4 * pow(pi,2) * pow(a,3))/ GM)
}

orbital_velocity_ave <- function(GM,a){
   #circ = 2pi*r
		period = sqrt((4 * pow(pi,2) * pow(a,3))/ GM)
		circ = 2 * pi * a
		circ / period
}
orbital_velocity_circ <- function(GM,r){
		sqrt(GM/r)
}



ang_disp_from_time <- function(GM,a,time,planet_radius=Kerbin_radius){
		period = sqrt((4 * pow(pi,2) * pow(a,3))/ GM)
		(time/period) * 2 * pi
}

kin_energy <- function(v,m){
		(m * v * v) /2
}
pot_grav_energy <- function(r,m,GM){
		-1 * (GM * m) / r
}

energy_from_vel_rad_mass <- function(v,r,m,GM){
		v_sqr = v * v
		kinectic= (m * v_sqr)/ 2
		potential= -1 * (GM * m)/r
		kinectic + potential	
}

escape_vel_from_alt <- function(r, GM){
		sqrt((2 * GM) / r)
}
leave_soi_from_alt <- function(r,GM,SOI){
		Rp = r
		Ra = SOI
		GM_2 = GM * 2
		Vp = sqrt( (GM_2 * Ra) / (Rp * (Rp + Ra)) )
		Vp
}

orbit_from_api_ap <- function(Va, Ra, GM){
		GM_2 = GM * 2
		rv2 = Ra * pow(Va,2)
		Rp = Ra / ( (GM_2 / rv2) - 1 )
		Vp = sqrt( (GM_2 * Ra) / (Rp * (Rp + Ra)) )

		major_axis = (Rp + Ra) / 2
		ecc = (Rp * pow(Vp,2))/GM
		period = period_from_semi_major(GM,major_axis)
		list (Va= Va, Ra = Ra, Rp = Rp, Vp = Vp,ecc= ecc, 
					major_axis=major_axis,
					period=period)
}

orbit_from_peri_ap <- function(Vp, Rp, GM){
		GM_2 = GM * 2
		rv2 = Rp * pow(Vp,2)
		Ra = Rp / ( (GM_2 / rv2) - 1 )
		Va = sqrt( (GM_2 * Rp) / (Ra * (Ra + Rp)) )

		major_axis = (Rp + Ra) / 2
		ecc = (Rp * pow(Vp,2))/GM
		period = period_from_semi_major(GM,major_axis)
		list (Va= Va, Ra = Ra, Rp = Rp, Vp = Vp,ecc= ecc, 
					major_axis=major_axis,
					period=period)
}

mean_motion_from_major_axis <- function(a, GM){
		sqrt(GM/ pow(a,3))
}

# equals 2*pi for one orbit
# since perigee
mean_anomoly <- function(M_naught, n, time, time_naught){
		n * (time - time_naught) + M_naught
}

ecc_anom <- function(true_anom, ecc){
		acos( (ecc + cos(true_anom)) / ( 1 + ecc * cos(true_anom)))
}

mean_anom_from_true_anom_ecc <- function(true_anom, ecc){
		E = ecc_anom(true_anom, ecc)
		E - ( ecc * sin(E))
}


true_anom_from_ecc_mean_anom <- function(M,ecc){
		M + (2 * ecc * sin(M)) + 1.25 * pow(ecc,2) * sin(2*M)
}
true_anom_from_axis_ecc_radius <- function(major_axis,ecc,radius){

		num = ((major_axis * ( 1 - ecc * ecc))/radius) -1
		acos(num/ecc)
}


vel_from_semi_axis_radius <- function(major_axis, radius, GM) {
		inv_r_2= 2/radius
		inv_a = 1/major_axis

		sqrt(GM * (inv_r_2 - inv_a) )
}

delta_v_inc_change <- function(V,theta){
		2 * V * sin(theta/2)
}

# transfer from A to B
hoff_trans_stats <- function(rA, rB, GM,mass=1){
		trans_axis = (rA + rB)/2
		v_iA = orbital_velocity_circ(GM,rA) # initial A
		v_fB = orbital_velocity_ave(GM,rB)  # final B
		v_txa = vel_from_semi_axis_radius(major_axis=trans_axis,radius=rA,GM=GM)
		v_txb = vel_from_semi_axis_radius(major_axis=trans_axis,radius=rB,GM=GM)
		list( init_delta_v = v_txa - v_iA,
				  final_delta_v = v_fB - v_txb,
					total_delta_v = (v_txa - v_iA) + (v_fB - v_txb),
					energy_A = energy_from_vel_rad_mass(v_iA,rA,m=1,GM=Kerbin_GM),
					energy_txa = energy_from_vel_rad_mass(v_txa,rA,m=1,GM=Kerbin_GM),
					energy_txb = energy_from_vel_rad_mass(v_txb,rB,m=1,GM=Kerbin_GM),
					energy_fB = energy_from_vel_rad_mass(v_fB,rB,m=1,GM=Kerbin_GM))

}




one_tang_burn <- function(rA,rB,rTx,GM){
		atx = (rA + rTx) / 2 
		ecc = 1 - (rA/atx)
		v_iA = orbital_velocity_circ(GM,rA) # initial A
		v_fB = orbital_velocity_ave(GM,rB)  # final B
		vtxA = vel_from_semi_axis_radius(major_axis=atx, radius=rA, GM=Kerbin_GM) 
		vtxB = vel_from_semi_axis_radius(major_axis=atx, radius=rB, GM=Kerbin_GM) 
    p_B = period_from_semi_major(GM=GM,rB) # w = 2pi/P
		w_b = ang_vel_from_period(p_B)

		true_anom_at_2burn = true_anom_from_axis_ecc_radius(major_axis=atx,ecc=ecc,radius=rB)

		esinv = ecc * sin(true_anom_at_2burn)
		ecosv_1 = (1 + ecc * cos(true_anom_at_2burn))
		flight_path_angle_second_burn = atan(esinv/ecosv_1)
		final_delta_v = sqrt(vtxB * vtxB + v_fB * v_fB - (2 * vtxB * v_fB * cos(flight_path_angle_second_burn)))
		ecc_anom_at_2burn = ecc_anom(true_anom=true_anom_at_2burn, ecc=ecc)
		E = ecc_anom_at_2burn
		TOF = (E- ecc * sin(E)) * sqrt(pow(atx,3)/GM)
		sum_delta_v = (vtxA - v_iA) + final_delta_v
		b_angle_disp = TOF * w_b

		list(TOF=TOF, total_delta_v = sum_delta_v,phi=flight_path_angle_second_burn,
				 B_angle_disp= b_angle_disp)
}

delta_v_polar_inc <- function(inc_needed=pi/2, rPark, rAdjust,GM=Kerbin_GM){
		trans_axis = (rPark + rAdjust)

		vPark = vel_from_semi_axis_radius(major_axis=rPark,radius=rPark,GM=GM)
		vTxPark = vel_from_semi_axis_radius(major_axis=trans_axis,radius=rPark,GM=GM)
		vTxAdjust = vel_from_semi_axis_radius(major_axis=trans_axis,radius=rAdjust,GM=GM)
		delta_v_to_tx = vTxPark - vPark


    delta_v_adj_tx = delta_v_inc_change ( V = vTxAdjust, theta = inc_needed )
    delta_v_adj_park = delta_v_inc_change ( V = vPark, theta = inc_needed )

		list( park_2_tx_delta_v = delta_v_to_tx,
				  inc_delta_v = delta_v_adj_tx,
					peri_vel_tx = vTxAdjust,
					total_delta_v_with_tx = 2 * delta_v_to_tx + delta_v_adj_tx,
					delta_v_adj_inc_in_park = delta_v_adj_park,
					delta_v_savings = delta_v_adj_park - (2 * delta_v_to_tx + delta_v_adj_tx)  )

}



