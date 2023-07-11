#  /usr/bin/env LD_LIBRARY_PATH=/nix/store/vaxgqbm6h3zvhpgk9xqqk97hqjp9gzvs-gcc-12.2.0-lib/lib:/nix/store/dj5anrpwcn0p6h3xnqyr5mz30fc7l825-gcc-12.2.0/lib:$LD_LIBRARY_PATH:/run/current-system/sw/lib python ./download.py
from huggingface_hub import hf_hub_download, snapshot_download
from transformers import AutoModelForCausalLM, AutoTokenizer
from transformers import LlamaForCausalLM, LlamaTokenizer
from transformers import BitsAndBytesConfig
from accelerate import init_empty_weights
import torch

default_args = {
  "force_download": False,
  "resume_download": True,
  # "low_cpu_mem_usage": True,
  # "device_map": "meta",
  # "torch_dtype": torch.bfloat16,
  # "torch_dtype": torch.float16,
  # "quantization_config": BitsAndBytesConfig(load_in_8bit=True),
}

snapshot_download(repo_id="replit/replit-code-v1-3b", **default_args)
# tokenizer = AutoTokenizer.from_pretrained('replit/replit-code-v1-3b', trust_remote_code=True, **default_args)
# model = AutoModelForCausalLM.from_pretrained('replit/replit-code-v1-3b', trust_remote_code=True, **default_args)


cerebras_gpt3_parameters = [ "256M", "590M", "2.7B", "6.7B", "13B", "1.3B", "111M" ]
for p_count in cerebras_gpt3_parameters:
    repo = f"cerebras/Cerebras-GPT-{p_count}"
    print(repo)
    snapshot_download(repo_id=repo, **default_args)
    # tokenizer = AutoTokenizer.from_pretrained(repo, **default_args)
    # model = AutoModelForCausalLM.from_pretrained(repo, **default_args)


pythia_models = [ "70m", "160m", "410m", "1b", "1.4b", "2.8b", "6.9b", "12b" ]
for model in pythia_models:
    repo = f"EleutherAI/pythia-{model}"
    repo_deduped = f"{repo}-deduped"

    for src in [repo, repo_deduped]:
      print(src)
      snapshot_download(repo_id=src, **default_args)
      # tokenizer = AutoTokenizer.from_pretrained(src, revision="step143000", **default_args)
      # model = AutoModelForCausalLM.from_pretrained(src, revision="step143000", **default_args)


openlm_models_preview = [ "200bt", "300bt" ]
for preview_model in openlm_models_preview:
    repo = f"openlm-research/open_llama_7b_preview_{preview_model}"
    print(repo)
    snapshot_download(repo_id=repo, **default_args)
    # tokenizer = LlamaTokenizer.from_pretrained(repo, **default_args)
    # model = LlamaForCausalLM.from_pretrained(repo, **default_args)



together_computer_incite = [
    # Apache 2.0
    "Base-3B-v1",
    "Chat-3B-v1",
    "Instruct-3B-v1",
    "Base-7B-v0.1",
    "Chat-7B-v0.1",
    "Instruct-7B-v0.1",
]
for model in together_computer_incite:
    repo = f"togethercomputer/RedPajama-INCITE-{model}"
    print(repo)
    snapshot_download(repo_id=repo, **default_args)
    # tokenizer = AutoTokenizer.from_pretrained(repo, **default_args)
    # torch_dtype=torch.float16
    # model = AutoModelForCausalLM.from_pretrained(repo, **default_args)


mpt_models = [
    # Apache-2.0
    "mpt-7b",
    "mpt-7b-storywriter",
    # CC-By-SA-3.0
    "mpt-7b-instruct",
    # CC-By-NC-SA-4.0 (non-commercial use only)
    "mpt-7b-chat",
]
for model in mpt_models:
    repo = f"mosaicml/{model}"
    print(repo)
    snapshot_download(repo_id=repo, **default_args)
    # tokenizer = AutoTokenizer.from_pretrained("EleutherAI/gpt-neox-20b", **default_args)
    # torch_dtype=torch.float16
    # model = AutoModelForCausalLM.from_pretrained(repo, trust_remote_code=True, **default_args)
