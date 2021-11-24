import actr
import math
import numbers



# ---------------------------------------------------------------------------------------------------------

"""
def onehit_bj_number_sims(a,b):

    if isinstance(b,numbers.Number) and isinstance(a,numbers.Number):
        return (- ( abs(a - b) / max(a,b)))
    else:
        return False    

    --------------------------------------------------------------------

    actr.add_command("1hit-bj-number-sims",onehit_bj_number_sims,
                 "Similarity between numbers for 1-hit blackjack task.")

    actr.mod_chunk("goal","arg1",person,"arg2",location,"state","test")

    response_time = actr.run(30)[0]
    response = actr.chunk_slot_value(actr.buffer_read("goal"),"state")

    --------------------------------------------------------------------

    goal = actr.define_chunks(["state","add-attribute","name",name,"value", (value + offset)])[0]
    actr.goal_focus(goal)
    actr.run(20)

"""

actr.load_act_r_model("IFT703_Highway-draving:modele_zero.lisp")

trials = []
data_set = [[5,2],[0,2],
            [6,1],[2,3]]


# creation of globals
#model_speed    = ?
#model_weight   = ?
#model_position = ?  
#
#accident = None
#accident_position = ?  
#accident_rel_speed = ?
#
#car1 = None
#car1_position   = ?
#car1_rel_speed  = ?
#
#car2 = None
#car2_position   = ?
#car2_rel_speed  = ?
#
#car3 = None
#car3_position   = ?
#car3_rel_speed  = ?

class trial():
     def __init__(self,block,addend1,addend2,visible=None):
        self.block = block
        self.addend1 = addend1
        self.addend2 = addend2
        self.text = str(addend1) + " + " + str(addend2)
        self.visible = False


def fonction_truc():
    global trials
    trials[0].time = (actr.get_time(True) - trials[0].start) / 1000.0

def game0():
    """
    Establish every global variables that will be used during the program
    """
    #global deck1,deck2,opponent_threshold,opponent_rule,opponent_feedback
    
    actr.reset()
    global trials
    trials = []
    result = []
    #road_grip = 1

    print("coucou")

    for i in range(3):
        trials.append(list(map(lambda x: trial(i,*x,visible=True), actr.permute_list(data_set))))


    actr.add_command("highway-response", fonction_truc,
                     "Zbrodoff task key press response monitor")
    actr.monitor_command("output-key","highway-response")
    
    w = actr.open_exp_window("Alpha-arithmetic Experiment", visible=trial.visible)
    if run_model:
        actr.install_device(w)
    actr.add_text_to_exp_window("WESH", trial.text, x=100, y=150)
    trial[0].start = actr.get_time(run_model)

    if run_model :
        actr.run(1000)
    
    actr.remove_command_monitor("output-key","highway-response")
    actr.remove_command("highway-response")

    print("\n\n\n\n\n\n\n\nDone\n\n\n\n")


    #voiture_speed    = ?
    #voiture_weight   = ?
    #voiture_position = ?   
    #
    #accident_rel_speed = ?
    #accident_position  = ?  

    
def learning(nb_average, nb_try=20, graph=True,game=game0):
    """
    Create all the support for the simulation and display graphs
    :param  nb_average  : how many tries do we make to compute the average (y-axis)
    :param  nb_try      : how many tries do we have to do in a row (x-axis)
    :param  graph       : do we display the graph
    :param  game        : which game do we have to play
    """

    data = []*nb_try

    # need_to_remove = add_key_monitor() ?

    for i in range(nb_average):
        actr.reset()
        game()
        data = list(map(lambda x,y: sum_lists(x,y),data,blocks(nb_try,5)))

    #if need_to_remove:  ?
    #    remove_key_monitor()  ?

    percentages = list(map(lambda x: x[0] / n / 5,data))

    if graph:
        draw_graph(percentages)

    return [[sum(percentages[0:5])/5,
             sum(percentages[5:10])/5,
             sum(percentages[10:15])/5,
             sum(percentages[15:20])/5], # TODO : a mettre à jour selon la var nb_try
             percentages]

# Pour print me graph de la fonction learning
def sum_lists(x,y):
    return list(map(lambda v,w: v + w,x,y))

def blocks(blocks,block_size):
    """
    Runs the simulation n times
    :param  blocks      : The number of blocks to run
    :param  block_size  : How many simulation to play in one block
    """
    
    res = []
    # need_to_remove = add_key_monitor()  ?

    for i in range(blocks):
        res.append(setupHighway(...))

    # if need_to_remove:  ?
    #   remove_key_monitor()

    return res


def setupHighway(highway, print_game=False):
    """
    Set the simulation
    :param  highway     : The number of simulation to run
    :param  print_game  : Whether or not to print the details of each simulation
    """
    crash = False
    # need_to_remove = add_key_monitor() ?

    for hw in range(highway):
        a = 0
        #setupPosition( notre voiture )
        #setupAccident( l accident)

        # TODO : faire tout le programme de conduite ici
    

    return crash

# ---------------------------------------------------------------------------------------------------------
