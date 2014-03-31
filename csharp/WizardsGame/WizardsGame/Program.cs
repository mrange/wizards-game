using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace WizardsGame
{
    static class Program
    {
        static string LinesToString (this IEnumerable<string> lines)
        {
            var sb = new StringBuilder ();
            foreach (var line in lines)
            {
                sb.AppendLine (line);
            }
            return sb.ToString ();
        }

        static void PrintLines (this IEnumerable<string> lines)
        {
            Console.Write (lines.LinesToString ());
        }

        static string FormatWith(this string format, params object[] args)
        {
            return string.Format(format, args);
        }

        static string CleanUpString(this string v)
        {
            return v.ToLowerInvariant().Trim();
        }

        static string AsString (this CommandKey commandKey)
        {
            return commandKey.ToString().CleanUpString();
        }

        enum RoomKey        { LivingRoom , Garden , Attic }
        enum ObjectKey      { Whiskey , Bucket , Chain , Frog }
        enum CommandKey     { Look , Walk , Pickup , Inventory }
        enum ExitKey        { Door , Ladder }
        enum DirectionKey   { West , Upstairs , East , Downstairs }

        class EdgeDescriptor
        {
            public EdgeDescriptor (ExitKey exit, DirectionKey direction)
            {
                Exit        = exit      ;
                Direction   = direction ;
            }

            public ExitKey      Exit        ;
            public DirectionKey Direction   ;
        }

        static Dictionary<RoomKey, string> nodes = new Dictionary<RoomKey, string>
        {
            { RoomKey.LivingRoom    , "You are in the living room.\nA wizard is snoring loudly on the couch\n"  },
            { RoomKey.Garden        , "You are in a beautiful garden.\nThere is a well in front of you\n"       },
            { RoomKey.Attic         , "You are in the attic.\nThere is a giant welding torch in the corner\n"   },
        };

        static Dictionary<RoomKey, Dictionary<RoomKey, EdgeDescriptor>> edges = new Dictionary<RoomKey, Dictionary<RoomKey, EdgeDescriptor>>
        {
            {
                RoomKey.LivingRoom  , new Dictionary<RoomKey, EdgeDescriptor>
                {
                    { RoomKey.Garden    , new EdgeDescriptor (ExitKey.Door, DirectionKey.West)          },
                    { RoomKey.Attic     , new EdgeDescriptor (ExitKey.Ladder, DirectionKey.Upstairs)    },
                }
            },
            {
                RoomKey.Garden      , new Dictionary<RoomKey, EdgeDescriptor>
                {
                    { RoomKey.LivingRoom, new EdgeDescriptor (ExitKey.Door, DirectionKey.East)          },
                }
            },
            {
                RoomKey.Attic       , new Dictionary<RoomKey, EdgeDescriptor>
                {
                    { RoomKey.LivingRoom, new EdgeDescriptor (ExitKey.Ladder, DirectionKey.Downstairs)  },
                }
            },
        };

        static Dictionary<RoomKey, HashSet<ObjectKey>> objectLocations = new Dictionary<RoomKey, HashSet<ObjectKey>>
        {
            { RoomKey.LivingRoom, new HashSet<ObjectKey> { ObjectKey.Whiskey   , ObjectKey.Bucket  }},
            { RoomKey.Garden    , new HashSet<ObjectKey> { ObjectKey.Chain     , ObjectKey.Frog    }},
            { RoomKey.Attic     , new HashSet<ObjectKey> {                                         }},
        };

        static Dictionary<string, ObjectKey> objectAliases = new Dictionary<string, ObjectKey>
        {
            { "whiskey"   , ObjectKey.Whiskey   },
            { "bucket"    , ObjectKey.Bucket    },
            { "chain"     , ObjectKey.Chain     },
            { "frog"      , ObjectKey.Frog      },
        };

        static Dictionary<string, DirectionKey> directionAliases = new Dictionary<string, DirectionKey>
        {
            { "west"      , DirectionKey.West       },
            { "upstairs"  , DirectionKey.Upstairs   },
            { "east"      , DirectionKey.East       },
            { "downstairs", DirectionKey.Downstairs },
        };

        static Dictionary<string, Action<string>> commands = new Dictionary<string, Action<string>>
        {
            { CommandKey.Look.AsString()        , arg => Look()         },
            { CommandKey.Inventory.AsString()   , arg => Inventory()    },
            { CommandKey.Pickup.AsString()      , arg =>
                {
                    ObjectKey objectKey;
                    if(objectAliases.TryGetValue(arg, out objectKey))
                    {
                        Pickup(objectKey);
                    }
                    else
                    {
                        Console.WriteLine ("You can't find the {0}", arg);
                    }
                }
            },
            { CommandKey.Walk.AsString()        , arg =>
                {
                    DirectionKey directionKey;
                    if(directionAliases.TryGetValue(arg, out directionKey))
                    {
                        Walk(directionKey);
                    }
                    else
                    {
                        Console.WriteLine ("You can't walk that way");
                    }
                }
            },
        };

        static HashSet<ObjectKey> inv = new HashSet<ObjectKey>();
        static RoomKey location = RoomKey.LivingRoom;

        static IEnumerable<string> LookImpl()
        {
            yield return nodes[location];

            foreach (var kv in edges[location])
            {
                var edge = kv.Value;
                yield return "There is a {0} going {1} from here.".FormatWith(edge.Exit, edge.Direction);
            }

            foreach (var o in objectLocations[location])
            {
                yield return "You see a {0} on the floor".FormatWith(o);
            }

        }

        static void Look()
        {
            LookImpl().PrintLines();
        }

        static void Walk(DirectionKey direction)
        {
            var next = edges[location].FirstOrDefault(kv => kv.Value.Direction == direction);

            if (next.Value == null)
            {
                Console.WriteLine("You cannot go that way");
            }   
            else
            {
                location = next.Key;
                Look();
            }
        }

        static void Pickup (ObjectKey objectKey)
        {
            var os = objectLocations[location];
            if (os.Remove(objectKey))
            {
                inv.Add(objectKey);
                Console.WriteLine("You are now carrying the {0}", objectKey);
            }
            else
            {
                Console.WriteLine("You can't get that");
            }
        }

        static IEnumerable<string> InventoryImpl()
        {
            yield return "You are carrying:";

            if(inv.Count == 0)
            {
              yield return "  Nothing";
            }
            else
            {
                foreach (var o in inv)
                {
                    yield return "A {0}".FormatWith(o);
                }
            }
        }

        static void Inventory()
        {
            InventoryImpl().PrintLines();
        }

        static bool AcceptLine (string line)
        {
            var lowerLine = line.CleanUpString();
            if (lowerLine == "quit")
            {
                return true;
            }

            var args = lowerLine.Split(' ');
            if(args.Length < 1)
            {
                return false;
            }

            Action<string> action;
            if(commands.TryGetValue(args[0], out action))
            {
                action(args.Length < 2 ? "" : args[1]);
            }
            else
            {
                Console.WriteLine("You can't do that");
            }

            return false;
        }

        static void Main (string[] args)
        {
            var exit = false;
            Look();
            while (!exit)
            {
                Console.Write("~> ");
                exit = AcceptLine(Console.ReadLine());
            }
        }


    }
}
