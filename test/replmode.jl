using Test
Base.include(@__MODULE__, joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testhelpers", "FakePTYs.jl"))
import .FakePTYs: open_fake_pty

slave, master = open_fake_pty()

CTRL_C = '\x03'

function run_repl_test(test_script)
    slave, master = open_fake_pty()
    # Start a julia process
    p = run(`$(Base.julia_cmd()) --history-file=no --startup-file=no`, slave, slave, slave; wait=false)
    
    # Read until the prompt
    readuntil(master, "julia>", keep=true)
    done = false
    repl_output_buffer = IOBuffer()

    # A task that just keeps reading the output
    @async begin
        while true
            done && break
            write(repl_output_buffer, readavailable(master))
        end
    end

    # Execute our "script"
    for l in split(test_script, '\n'; keepempty=false)
        write(master, l, '\n')
    end

    # Let the REPL exit
    write(master, "exit()\n")
    wait(p)
    done = true

    # Gather the output
    repl_output = String(take!(repl_output_buffer))
    return split(repl_output, '\n'; keepempty=false)
end


test_script1 = """
using Rewrite

=
1 + 1
"""*CTRL_C

test_script1p = """
using Rewrite

normalize(@term 1 + 1)
"""*CTRL_C

test_script2 = """
using Rewrite

=
1 + 1
"""*CTRL_C

out1 = run_repl_test(test_script1);
out1p = run_repl_test(test_script1p);

test_script2p = """
using Rewrite

normalize(@term 1 + 1)
"""*CTRL_C

out2 = run_repl_test(test_script2);
out2p = run_repl_test(test_script2p);


test_script3 = """
using Rewrite

=
cos(x)^2 + sin(x)^2
"""*CTRL_C

test_script3p = """
using Rewrite

normalize(@term cos(x)^2 + sin(x)^2)
"""*CTRL_C

out3 = run_repl_test(test_script3);
out3p = run_repl_test(test_script3p);

@testset "REPL Mode" begin
    @test out1[end-7] == out1p[end-7]
    println("Testing REPL...")  # FIXME: avoids CI timeout
    @test out2[end-7] == out2p[end-7]
    println("Testing REPL...")  # FIXME: avoids CI timeout
    @test out3[end-7] == out3p[end-7]
end
