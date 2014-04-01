#include <cstddef>
#include <cstdio>

#include <functional>
#include <iostream>
#include <map>
#include <map>
#include <set>
#include <string>

namespace
{
    template<typename TKey, typename TValue>
    std::map<TValue, TKey> reverse (std::map<TKey, TValue> const & m)
    {
        std::map<TValue, TKey> r;

        for (auto & kv : m)
        {
            r.emplace (kv.second, kv.first);
        }

        return r;
    }

    std::pair<std::string, std::string> split (std::string const & s)
    {
        auto splitter = s.find (' ');
        if (splitter == std::string::npos)
        {
            return std::make_pair (s, "");
        }

        auto first = s.substr (0, splitter);
        auto second = s.substr (splitter + 1);

        return std::make_pair (std::move (first), std::move (second));
    }

    enum room_key       { living_room, garden, attic        };
    enum object_key     { whiskey, bucket, chain, frog      };
    enum exit_key       { door, ladder                      };
    enum direction_key  { west, upstairs, east, downstairs  };

    using object_alias_map = std::map<object_key, std::string>;
    object_alias_map const object_aliases
    {
        { whiskey   , "whiskey" },
        { bucket    , "bucket"  },
        { chain     , "chain"   },
        { frog      , "frog"    },
    };

    using exit_alias_map = std::map<exit_key, std::string>;
    exit_alias_map const exit_aliases
    {
        { door      , "door"    },
        { ladder    , "ladder"  },
    };

    using direction_alias_map = std::map<direction_key, std::string>;
    direction_alias_map const direction_aliases
    {
        { west          , "west"      },
        { upstairs      , "upstairs"  },
        { east          , "east"      },
        { downstairs    , "downstairs"},
    };

    auto const reverse_object_aliases   = reverse (object_aliases);
    auto const reverse_direction_aliases= reverse (direction_aliases);

    using node_map = std::map<room_key, std::string>;

    node_map const nodes
    {
        { living_room   , "You are in the living room.\nA wizard is snoring loudly on the couch\n"  },
        { garden        , "You are in a beautiful garden.\nThere is a well in front of you\n"       },
        { attic         , "You are in the attic.\nThere is a giant welding torch in the corner\n"   },
    };

    struct edge_descriptor
    {
        exit_key        exit        ;
        direction_key   direction   ;
    };

    using edge_map = std::map<room_key, std::map<room_key, edge_descriptor>>;
    edge_map const edges
    {
        {
            living_room ,
            {
                { garden        , { door    , west      } },
                { attic         , { ladder  , upstairs  } },
            }
        },
        {
            garden      ,
            {
                { living_room   , { door    , east      } },
            }
        },
        {
            attic       ,
            {
                { living_room   , { ladder  , upstairs  } },
            }
        },
    };

    using object_set = std::set<object_key>;
    using object_location_map = std::map<room_key, object_set>;

    object_location_map object_locations
    {
        { living_room   , {whiskey  , bucket    }},
        { garden        , {chain    , frog      }},
        { attic         , {                     }},
    };

    auto        location = living_room  ;
    object_set  inventory               ;

    void look ()
    {
        std::cout << nodes.at (location) << std::endl;

        for (auto & kv : edges.at (location))
        {
            auto & edge = kv.second;
            std::cout
                << "There is a "
                << exit_aliases.at (edge.exit)
                << " going "
                << direction_aliases.at (edge.direction)
                << " from here."
                << std::endl
                ;
        }

        for (auto & object : object_locations.at (location))
        {
            std::cout
                << "You see a "
                << object_aliases.at (object)
                << " on the floor"
                << std::endl
                ;
        }

    }

    void show_inventory ()
    {
        std::cout
            << "You're carrying:"
            << std::endl
            ;

        if (inventory.empty ())
        {
            std::cout
                << "  Nothing"
                << std::endl
                ;
            return;
        }

        for (auto & object_key : inventory)
        {
            std::cout
                << "  "
                << object_aliases.at (object_key)
                << std::endl
                ;
        }
    }

    void walk (direction_key direction)
    {
        auto & next = edges.at (location);

        for (auto & kv : next)
        {
            auto & ed = kv.second;
            if (ed.direction == direction)
            {
                location = kv.first;
                look ();
                return;
            }
        }

        std::cout << "You can't go that way" << std::endl;
    }


    void pickup (object_key object)
    {
        auto & set = object_locations.at (location);

        if (!set.erase (object))
        {
            std::cout << "You can't get that" << std::endl;
        }

        inventory.insert (object);
        std::cout
            << "You are now carrying the "
            << object_aliases.at (object)
            << std::endl
            ;
    }
    using action = std::function<void (std::string const & arg)>;

    using command_map = std::map<std::string, action>;

    command_map commands
    {
        { "look"        , [] (std::string const & arg) { look (); } },
        { "inventory"   , [] (std::string const & arg) { show_inventory (); } },
        { "pickup"      , [] (std::string const & arg)
            {
                auto find = reverse_object_aliases.find (arg);
                if (find == reverse_object_aliases.end ())
                {
                    std::cout
                        << "You can't find the "
                        << arg
                        << std::endl
                        ;
                    return;
                }

                pickup (find->second);

            }
        },
        { "walk"        , [] (std::string const & arg)
            {
                auto find = reverse_direction_aliases.find (arg);
                if (find == reverse_direction_aliases.end ())
                {
                    std::cout
                        << "You can't walk that way"
                        << std::endl
                        ;
                    return;
                }

                walk (find->second);

            }
        },
    };

    bool accept_line (std::string const & line)
    {
        auto p = split (line);

        auto & cmd = p.first;
        auto & arg = p.second;

        if (cmd == "quit")
        {
            return true;
        }

        auto find = commands.find (cmd);
        if (find == commands.end ())
        {
            std::cout
                << "You can't do that"
                << std::endl
                ;
            return false;
        }

        auto & action = find->second;

        action (arg);

        return false;
    }
}

int main()
{
    auto exit = false;

    look ();

    while (!exit)
    {
        std::cout << "~> ";

        std::string line;
        if (std::getline (std::cin, line))
        {
            exit = accept_line (line);
        }
        else
        {
            exit = true;
        }

    }

    look ();
    return 0;
}

