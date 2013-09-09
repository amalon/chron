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

int usage(const char *arg0, int ret)
{
	std::cerr << "Usage: " << arg0 << " <file>" << std::endl;
	return ret;
}

int main(int argc, char **argv)
{
	int err;

	if (argc < 2) {
		std::cerr << "No database file provided" << std::endl;
		return usage(argv[0], 1);
	}
	const char *filename = argv[1];

	chron::Database db(argv[0], filename);

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
