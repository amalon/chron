/*
 * chron/chron.cpp
 *
 * Copyright (C) 2012 James Hogan <james@albanarts.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details
 * (in the file called COPYING).
 *
 *
 * Command line test program of chron library.
 *
 */

#include <SWI-cpp.h>
#include <iostream>
#include <cassert>

#include <chron/chron.h>

#define ARRAY_SIZE(X) (sizeof(X) / sizeof(X[0]))

int main(int argc, char **argv)
{
	int err;
	char *args[] = {
		argv[0],
		(char *)"-q",	// silence SWI-prolog copyright information

		(char *)"-p",
		(char *)"chron=../src/",

		(char *)"-s",
		(char *)"../src/wrapper.pl",
	};
	PlEngine pl(ARRAY_SIZE(args), args);

	try {
		// use first event as epoch
		chron::Event epoch;
		chron::Chron::EventIterator epochit;
		if (!epochit) {
			std::cout << "no epoch" << std::endl;
			return 1;
		}
		epoch = *epochit;
		std::cout << "epoch " << epoch.name() << std::endl;

		// load database relative to the epoch
		chron::Chron db;
		err = db.load(epoch);
		if (err) {
			std::cerr << "load failed" << std::endl;
			return err;
		}
		std::cout << "db loaded" << std::endl;

		// iterate events
		for (chron::Chron::EventIterator eit(db); eit; ++eit) {
			chron::Event e = *eit;
			chron::Time diff = eit.time();
			std::cout << "event " << e.name() << " = " << diff << std::endl;
		}

		// iterate periods
		for (chron::Chron::PeriodIterator pit(db); pit; ++pit) {
			chron::Period p = *pit;
			chron::Time begin_diff = pit.begin_time();
			chron::Time end_diff = pit.end_time();
			std::cout << "period " << p.name() << " = (" << begin_diff << ") .. (" << end_diff << ")" << std::endl;
		}
	} catch (PlException e) {
		std::cerr << "Prolog Exception: "
			  << (const char *)e << std::endl;
		return 1;
	}

	return 0;
}
