<template>
  <page-skeleton>
    <template #breadcrumbs>
      <el-breadcrumb-item :to="{ name: 'recipe-list' }">
        My Recipes
      </el-breadcrumb-item>
      <el-breadcrumb-item :to="{ name: 'recipe-add' }">
        Add Recipe
      </el-breadcrumb-item>
    </template>
    <el-form ref="form" :model="form" label-width="120px">
      <el-form-item label="Name">
        <el-input v-model="form.name" />
      </el-form-item>
      <el-form-item label="Tags">
        <el-tooltip class="tooltip" effect="dark"
          placement="bottom" content="comma separated list">
          <el-input v-model="form.tags" />
        </el-tooltip>
      </el-form-item>
      <el-form-item label="Pictures">
        <el-upload
          action="#"
          list-type="picture-card"
          :auto-upload="false"
          :on-change="updateFileList"
          ref="fileInput">
          <template #default>
            <el-icon><i-ep-plus /></el-icon>
          </template>
          <template #file="{ file }">
            <img class="el-upload-list__item-thumbnail" :src="file.url" />
          </template>
        </el-upload>
      </el-form-item>
      <el-form-item>
        <el-button type="primary" @click="onSubmit">Create</el-button>
      </el-form-item>
    </el-form>
  </page-skeleton>
</template>

<script>
import PageSkeleton from "./PageSkeleton.vue"
import { postRecipe } from "../Api.js"

export default {
  name: 'RecipeAdd',
  components: {PageSkeleton},
  data() {
    return {
      form: {
        name: "",
        tags: "",
        fileList: [],
      },
    }
  },
  methods: {
    updateFileList(file, fileList) {
      this.$data.form.fileList = fileList
    },
    onSubmit() {
      postRecipe(
        this.$data.form.name, 
        this.$data.form.tags,
        this.$data.form.fileList.map(f => f.raw)
      )
        .then(() => {
          this.$router.push({ name: "recipe-list" })
        })
        .catch(err => {
          console.error(err)
          ElMessage("An error occured uploading the recipe. Check your network")
        })
    },
  },
}
</script>

<style>
</style>
